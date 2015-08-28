package banjo.eval;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Function;

import banjo.eval.expr.BindingInstance;
import banjo.eval.expr.ObjectLiteralInstance;
import banjo.eval.expr.RecursiveSlotInstance;
import banjo.eval.expr.SlotInstance;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.eval.util.LazyBoundValue;
import banjo.event.Event;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.ObjectLiteral;
import banjo.expr.core.Slot;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;
import banjo.expr.free.FreeIdentifier;
import banjo.expr.free.FreeProjection;
import banjo.expr.token.Identifier;
import banjo.value.Reaction;
import banjo.value.Reactive;
import banjo.value.Value;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Option;
import fj.data.TreeMap;

public class Environment implements Function<String, Option<BindingInstance>>, Reactive<Environment> {

	public final Environment rootEnvironment;
	public final Value rootObject;
	public final TreeMap<String, BindingInstance> bindings;
	public final boolean reactive;

	public Environment(Value rootObject, TreeMap<String, BindingInstance> bindings, Environment rootEnvironment) {
		this.bindings = bindings;
		this.rootObject = rootObject;
		this.reactive = rootObject.isReactive() || checkReactive(bindings);
		this.rootEnvironment = rootEnvironment == null ? this : rootEnvironment;
	}
	
	public Environment(Value rootObject, Environment rootEnvironment) {
		this(rootObject, TreeMap.empty(Ord.stringOrd), rootEnvironment);
	}
	
	public boolean checkReactive(TreeMap<String, BindingInstance> bindings) {
		boolean reactive = false;
		for(P2<String, BindingInstance> p : bindings) {
			if(p._2().isReactive()) {
				reactive = true;
				break;
			}
		}
		return reactive;
	}
	
	public Environment(TreeMap<String, FreeExpression> letBindings, Environment parentEnv) {
		// The trick here is that we mustn't "force" or calculate any value in the let until after we've set bindings
		this.bindings = letBindings.map(this::bindLazy).map(BindingInstance::let).union(parentEnv.bindings);
		this.rootObject = parentEnv.rootObject;
		this.reactive = parentEnv.isReactive() || checkReactive(bindings);
		this.rootEnvironment = parentEnv.rootEnvironment;
	}

	@Override
    public Option<BindingInstance> apply(String t) {
    	return bindings.get(t).orElse(() -> trySlot(t));
    }
	
	public Option<BindingInstance> trySlot(String t) {
		Value v = rootObject.slot(t);
		if(v instanceof SlotNotFound && ((SlotNotFound)v).id.equals(t)) {
			return Option.none();
		}
		return Option.some(BindingInstance.let(v));
	}

	@Override
	public String toString() {
	    return "("+bindings+") ⇒ ";
	}
	
	@Override
	public Reaction<Environment> react(Event event) {
		List<P2<String, BindingInstance>> pairs = List.iterableList(bindings);
		List<BindingInstance> deps = pairs.map(P2.__2());
		Reaction<List<BindingInstance>> reactions = Reaction.to(deps, event);
		boolean changedSlots = reactions.v != deps;
		TreeMap<String, BindingInstance> newSlots = 
				changedSlots ? TreeMap.treeMap(Ord.stringOrd, pairs.map(P2.__1()).zip(reactions.v)) :
				this.bindings;
		return Reaction.p(rootObject.react(event), reactions.from(newSlots)).map(P2.tuple(this::update));
	}

	@Override
	public boolean isReactive() {
		return reactive;
	}
	
	public Environment update(Value newRootObject, TreeMap<String, BindingInstance> newBindings) {
		if(newBindings == bindings && newRootObject == this.rootObject)
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
	
	public BindingInstance get(String id) {
		Option<BindingInstance> result = this.apply(id);
		if(result.isNone())
			return BindingInstance.let(unboundIdentifier(id));
		return result.some();
	}

	public Value getValue(String id) {
		Option<BindingInstance> result = this.apply(id);
		if(result.isNone())
			return unboundIdentifier(id);
		return result.some().value;
	}
	
	public static UnboundIdentifier unboundIdentifier(String id) {
	    return new UnboundIdentifier("No variable in scope named '"+id+"'");
    }

	public Environment let(List<P2<String, FreeExpression>> letBindings) {
		return let(TreeMap.treeMap(Ord.stringOrd, letBindings));
	}
	public Environment let(TreeMap<String, FreeExpression> letBindings) {
		return new Environment(letBindings, this);
	}
	
	public static List<P2<String, FreeExpression>> bindingsForPath(Path sourceFilePath) {
		return new ProjectLoader().loadLocalAndLibraryBindings(sourceFilePath).map(x -> P.p(x._1().id, FreeExpressionFactory.apply(x._2())));
	}

	public static final String JAVA_RUNTIME_ID = "java runtime";
	
	public Environment bind(List<P2<String, BindingInstance>> bindings) {
		return bind(TreeMap.<String,BindingInstance>treeMap(Ord.stringOrd, bindings));
	}
	
	public Environment bind(TreeMap<String, BindingInstance> bindings) {
		return update(rootObject, bindings.union(this.bindings));
	}
	
	public Environment(Path sourceFilePath) {
		this.bindings = TreeMap.empty(Ord.stringOrd);
		this.rootEnvironment = this;
		this.reactive = false;
		Value runtime = Value.fromClass(JavaRuntimeSupport.RootObject.class);
		List<P2<String, FreeExpression>> bindingPairs = bindingsForPath(sourceFilePath);
		FreeIdentifier freeEnvRef = new FreeIdentifier(List.nil(), Identifier.ENVIRONMENT.id);
		List<P2<String, SlotInstance>> rootSlots = bindingPairs.map(p -> P.p(
				p._1(), 
				new RecursiveSlotInstance(
						new Identifier(p._1()),
						Identifier.ENVIRONMENT,
						new FreeProjection(freeEnvRef, p._2()),
						this
				)
		));
		Value projectRootObject = new ObjectLiteralInstance(List.nil(),
				TreeMap.treeMap(Ord.stringOrd, rootSlots));
		this.rootObject = runtime.extendedWith(projectRootObject);
	}

	public static Environment forSourceFile(Path sourceFilePath) {
		return new Environment(sourceFilePath);
	}
	
	public static Environment forCurrentDirectory() {
		return forSourceFile(Paths.get("").toAbsolutePath());
	}

	public Environment bind(String id, BindingInstance binding) {
		return bind(TreeMap.<String,BindingInstance>empty(Ord.stringOrd).set(id, binding));
	}

	public Environment enterFunction(List<Identifier> formalArgs, List<Value> providedArgs, Option<Identifier> sourceObjectBinding, Value recurse, Value prevImpl) {
	    List<Identifier> missingArgNames = formalArgs.drop(providedArgs.length());
		final List<P2<String, BindingInstance>> missingArgBindings =
				missingArgNames.map(name -> P.p(name.id, BindingInstance.let(new ArgumentNotSupplied("Missing argument '"+name.id+"'"))));
		List<P2<String, BindingInstance>> argBindings = formalArgs
				.map(a -> a.id)
				.zip(providedArgs.map(BindingInstance::let)).append(missingArgBindings);
		final Option<P2<String, BindingInstance>> opt = sourceObjectBinding.map(n -> P.p(n.id, BindingInstance.functionSelf(recurse, prevImpl)));
		List<P2<String, BindingInstance>> recBinding =
				opt.toList();
		List<P2<String, BindingInstance>> allBindings = recBinding.append(argBindings);
	    return bind(TreeMap.treeMap(Ord.stringOrd, allBindings));
    }

}
