package banjo.eval.environment;

import static java.util.Objects.requireNonNull;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import banjo.eval.ArgumentNotSupplied;
import banjo.eval.Fail;
import banjo.eval.SlotNotFound;
import banjo.eval.UnboundIdentifier;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.eval.util.LazyBoundValue;
import banjo.event.PastEvent;
import banjo.expr.core.Call;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprFactory;
import banjo.expr.core.FunctionLiteral;
import banjo.expr.core.ObjectLiteral;
import banjo.expr.core.Projection;
import banjo.expr.core.Slot;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;
import banjo.expr.token.Identifier;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import banjo.value.Reaction;
import banjo.value.Reactive;
import banjo.value.SlotValue;
import banjo.value.Value;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

public class Environment implements Reactive<Environment> {
    public static final Environment EMPTY = new Environment(EmptyEnvironmentRoot.INSTANCE);
    private static final String LIB_PATH_SYS_PROPERTY = "banjo.path";

	public final Environment rootEnvironment;
	public final Value rootObject;
	public final TreeMap<String, Binding> bindings;
	public final boolean reactive;
	private ObservableEnvironment observable;

	public Environment(Value rootObject, TreeMap<String, Binding> bindings, Environment rootEnvironment) {
        this.bindings = requireNonNull(bindings);
        this.rootObject = requireNonNull(rootObject);
		this.reactive = rootObject.isReactive() || checkReactive(bindings);
        this.rootEnvironment = requireNonNull(rootEnvironment);
	}
	
	public Environment(Value rootObject, Environment rootEnvironment) {
		this(rootObject, TreeMap.empty(Ord.stringOrd), rootEnvironment);
	}

    /**
     * Constructor for the root environment of the project.
     */
    public Environment(Value rootObject) {
        this.bindings = TreeMap.empty(Ord.stringOrd);
        this.rootObject = rootObject;
        this.rootEnvironment = this;
        this.reactive = false;
    }
	
	public static Slot rootObjectSlot(Slot slot) {
        // The slot definitions in a project use the project root object
	    // as their base scope.  So, we'll use the projection feature to
	    // scope each slot to the "source object binding" for that slot.

        // Temporary identifier for the project root; this isn't visible
        // to the code because we use a projection to "reset" the
        // environment to use this as the base object
        Identifier projectBindingName = Identifier.__TMP;

        // If the slot has a sourceObjectBinding we'll have to preserve
        // that by making the slot a function that accepts the object
        // and returns its value.
        CoreExpr value = slot.sourceObjectBinding.map(
            name -> (CoreExpr) new Call(new FunctionLiteral(name, slot.value), projectBindingName))
            .orSome(slot.value);

        // Now return the transformed slot
        return new Slot(
            slot.name,
            Option.some(projectBindingName),
            new Projection(projectBindingName, value));
	    
	}

    public static Value createProjectRootObject(List<Path> rootPaths) {
        // Construct the project root object
        ObjectLiteral selfBoundObjectRoot = rootObjectFromPaths(rootPaths);
        return FreeExpressionFactory
            .apply(selfBoundObjectRoot)
            .apply(Environment.EMPTY);
    }

    public static ObjectLiteral rootObjectFromPaths(List<Path> rootPaths) {
        ObjectLiteral rootExpr = CoreExprFactory.INSTANCE.loadFromDirectories(rootPaths);
        List<Slot> newSlots = rootExpr.slots.map(Environment::rootObjectSlot);
        ObjectLiteral selfBoundObjectRoot = new ObjectLiteral(rootExpr.getSourceFileRanges(), newSlots);
        return selfBoundObjectRoot;
    }

    /**
     * Get the core library source search paths.
     */
    public static List<Path> getGlobalSourcePaths() {
        String searchPath = System.getProperty(LIB_PATH_SYS_PROPERTY, "");
        return List.list(searchPath.split(File.pathSeparator))
            .filter(s -> !s.isEmpty())
            .map(Paths::get);
    }

    /**
     * Find the full project source search path list for the given source file;
     * this includes the project the file is in plus the core library search
     * paths.
     */
    public static List<Path> projectSourcePathsForFile(Path sourceFile) {
        Option<Path> projectRoot = projectRootForPath(sourceFile);
        List<Path> coreLibraryPaths = getGlobalSourcePaths();
        return coreLibraryPaths.append(projectRoot.toList());
    }

    /**
     * Try to find the first parent folder of the given path which contains a
     * file/folder named ".banjo". This is considered to be the project root.
     * 
     * If no ".banjo" exists in the given path or a parent, this returns
     * Option.none().
     */
    public static Option<Path> projectRootForPath(Path path) {
        Path tryPath = path;
        while(tryPath != null) {
            if(Files.exists(tryPath.resolve(".banjo")))
                return Option.some(tryPath);
            tryPath = tryPath.getParent();
        }
        return Option.none();
    }

    /**
     * Return true if any binding value in this environment is reactive.
     */
	public boolean checkReactive(TreeMap<String, Binding> bindings) {
		boolean reactive = false;
		for(P2<String, Binding> p : bindings) {
			if(p._2().isReactive()) {
				reactive = true;
				break;
			}
		}
		return reactive;
	}
	
	public Environment(TreeMap<String, FreeExpression> letBindings, Environment parentEnv) {
		// The trick here is that we mustn't "force" or calculate any value in the let until after we've set bindings
		this.bindings = letBindings.map(this::bindLazy).map(Binding::let).union(parentEnv.bindings);
		this.rootObject = parentEnv.rootObject;
		this.reactive = parentEnv.isReactive() || checkReactive(bindings);
		this.rootEnvironment = parentEnv.rootEnvironment;
	}

	@Override
	public String toString() {
        return "(" + bindings + ") ⇒ ";
	}
	
	@Override
	public Reaction<Environment> react(PastEvent event) {
		if(!reactive)
			return Reaction.of(this);
		List<P2<String, Binding>> pairs = List.iterableList(bindings);
		List<Binding> deps = pairs.map(P2.__2());
		Reaction<List<Binding>> reactions = Reaction.to(deps, event);
		boolean changedSlots = reactions.v != deps;
		TreeMap<String, Binding> newSlots = 
				changedSlots ? TreeMap.treeMap(Ord.stringOrd, pairs.map(P2.__1()).zip(reactions.v)) :
				this.bindings;
		return Reaction.p(rootObject.react(event), reactions.from(newSlots)).map(P2.tuple(this::update));
	}

	@Override
	public boolean isReactive() {
		return reactive;
	}
	
	public Environment update(Value newRootObject, TreeMap<String, Binding> newBindings) {
		// TODO This check is O(n) space and time, but should be done in O(1) space
		if(ListUtil.elementsEq(newBindings.values(), bindings.values()) && newRootObject == this.rootObject)
			return this;
		return new Environment(newRootObject, newBindings, rootEnvironment);
	}
	
	public Value eval(CoreExpr ast) {
		return eval(FreeExpressionFactory.apply(ast));
	}

	public Value eval(final FreeExpression fx) {
	    return fx.apply(this);
    }
	
	public Value bindLazy(FreeExpression fe) {
		return new LazyBoundValue(fe, this);
	}
	
    public Value getValue(String id, Set<SourceFileRange> ranges) {
        Option<Binding> binding = bindings.get(id);
        if(binding.isSome()) {
            return binding.some().getValue();
        }

        return new SlotValue(rootObject, rootObject, id, ranges, unboundIdentifier(id, ranges));
	}
	
    public Fail unboundIdentifier(String id, Set<SourceFileRange> ranges) {
		if(this.rootObject == this.rootEnvironment.rootObject)
            return new UnboundIdentifier(id, ranges);
        return new SlotNotFound(id, ranges, rootObject);
    }

	public Environment let(List<P2<String, FreeExpression>> letBindings) {
		return let(TreeMap.treeMap(Ord.stringOrd, letBindings));
	}
	public Environment let(TreeMap<String, FreeExpression> letBindings) {
		return new Environment(letBindings, this);
	}
	
    public Environment bind(List<P2<String, Binding>> bindings) {
		return bind(TreeMap.<String,Binding>treeMap(Ord.stringOrd, bindings));
	}
	
	public Environment bind(TreeMap<String, Binding> bindings) {
		return update(rootObject, bindings.union(this.bindings));
	}
	
	public static Environment forSourceFile(Path sourceFilePath) {
        List<Path> paths = projectSourcePathsForFile(sourceFilePath.toAbsolutePath());
        Value rootObject = createProjectRootObject(paths);
        Value runtime = Value.fromClass(JavaRuntimeSupport.RootObject.class);
        Value environmentRoot = EmptyEnvironmentRoot.INSTANCE.extendedWith(runtime).extendedWith(rootObject);
        return new Environment(environmentRoot);
	}
	
	public static Environment forCurrentDirectory() {
        return forSourceFile(Paths.get(""));
	}

	public Environment bind(String id, Binding binding) {
		return bind(TreeMap.<String,Binding>empty(Ord.stringOrd).set(id, binding));
	}

	public Environment enterFunction(List<Identifier> formalArgs, List<Value> providedArgs, Option<Identifier> sourceObjectBinding, Value recurse, Value prevImpl) {
	    List<Identifier> missingArgNames = formalArgs.drop(providedArgs.length());
		final List<P2<String, Binding>> missingArgBindings =
				missingArgNames.map(name -> P.p(name.id, Binding.let(new ArgumentNotSupplied("Missing argument '"+name.id+"'"))));
		List<P2<String, Binding>> argBindings = formalArgs
				.map(a -> a.id)
				.zip(providedArgs.map(Binding::let)).append(missingArgBindings);
		final Option<P2<String, Binding>> opt = sourceObjectBinding.map(n -> P.p(n.id, 
				prevImpl == null ? Binding.functionSelf(recurse) : Binding.functionSelfWithBase(recurse, prevImpl)));
		List<P2<String, Binding>> recBinding = opt.toList();
		List<P2<String, Binding>> allBindings = recBinding.append(argBindings);
	    return bind(TreeMap.treeMap(Ord.stringOrd, allBindings));
    }

	public static final class ObservableEnvironment extends ObjectBinding<Environment> {
		public final ObservableValue<Value> rootObjectBinding;
		public final TreeMap<String, ObservableValue<Binding>> bindingsBinding;
		Environment environment;
		
		public ObservableEnvironment(Environment environment) {
			super();
			rootObjectBinding = environment.rootObject.toObservableValue();
			bindingsBinding = environment.bindings.map(Binding::toObservableValue);
			this.environment = environment;
		}
		
		@Override
		protected Environment computeValue() {
			return environment = environment.update(
					rootObjectBinding.getValue(), 
					bindingsBinding.map(ObservableValue::getValue));
		}
		
		@Override
		public void dispose() {
			unbind(bindingsBinding.values().map(ObservableValue.class::cast).cons(rootObjectBinding).array(ObservableValue[].class));
		}
		
	}
	@Override
    public ObservableValue<Environment> toObservableValue() {
		if(observable == null)
			observable = new ObservableEnvironment(this);
		return observable;
	}

}
