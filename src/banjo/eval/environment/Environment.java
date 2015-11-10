package banjo.eval.environment;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Function;

import banjo.eval.ArgumentNotSupplied;
import banjo.eval.Fail;
import banjo.eval.ProjectLoader;
import banjo.eval.SlotNotFound;
import banjo.eval.UnboundIdentifier;
import banjo.eval.expr.ObjectLiteralInstance;
import banjo.eval.expr.RecursiveSlotInstance;
import banjo.eval.expr.SlotInstance;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.eval.util.LazyBoundValue;
import banjo.event.PastEvent;
import banjo.expr.core.CoreExpr;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;
import banjo.expr.free.FreeIdentifier;
import banjo.expr.free.FreeProjection;
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
import fj.data.TreeMap;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

public class Environment implements Reactive<Environment> {

	public final Environment rootEnvironment;
	public final Value rootObject;
	public final TreeMap<String, Binding> bindings;
	public final boolean reactive;
	private ObservableEnvironment observable;

	public Environment(Value rootObject, TreeMap<String, Binding> bindings, Environment rootEnvironment) {
		this.bindings = bindings;
		this.rootObject = rootObject;
		this.reactive = rootObject.isReactive() || checkReactive(bindings);
		this.rootEnvironment = rootEnvironment == null ? this : rootEnvironment;
	}
	
	public Environment(Value rootObject, Environment rootEnvironment) {
		this(rootObject, TreeMap.empty(Ord.stringOrd), rootEnvironment);
	}
	
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
	    return "("+bindings+") ⇒ ";
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
	
	public Value getValue(String id) {
		// TODO Ideally this unbound identifier fallback value wouldn't be constructed unless it was really needed
		return bindings.get(id).map(Binding::getValue).orSome(() -> new SlotValue(rootObject, rootObject, id, unboundIdentifier(id)));
	}
	
	public Fail unboundIdentifier(String id) {
		if(this.rootObject == this.rootEnvironment.rootObject)
			return new UnboundIdentifier(id);
		return new SlotNotFound(id, rootObject);
    }

	public Environment let(List<P2<String, FreeExpression>> letBindings) {
		return let(TreeMap.treeMap(Ord.stringOrd, letBindings));
	}
	public Environment let(TreeMap<String, FreeExpression> letBindings) {
		return new Environment(letBindings, this);
	}
	
	public static final String JAVA_RUNTIME_ID = "java runtime";
	
	public Environment bind(List<P2<String, Binding>> bindings) {
		return bind(TreeMap.<String,Binding>treeMap(Ord.stringOrd, bindings));
	}
	
	public Environment bind(TreeMap<String, Binding> bindings) {
		return update(rootObject, bindings.union(this.bindings));
	}
	
	public Environment(Path sourceFilePath) {
		this.bindings = TreeMap.empty(Ord.stringOrd);
		this.rootEnvironment = this;
		this.reactive = false;
		Value runtime = Value.fromClass(JavaRuntimeSupport.RootObject.class);
		List<P2<String, SlotInstance>> rootSlots =
				new ProjectLoader().loadLocalAndLibraryBindings(sourceFilePath).map(slot -> P.p(
							slot.name.id,
							new RecursiveSlotInstance(
								slot.name,
								// We need an identifier for the object so we can project into it, so we use this "special" identifier
								// TODO Ideally this would be a hygienic name instead
								slot.sourceObjectBinding.orSome(Identifier.ENVIRONMENT),
								new FreeProjection(new FreeIdentifier(slot.sourceObjectBinding.orSome(Identifier.ENVIRONMENT)), FreeExpressionFactory.apply(slot.value)),
								this
							)	
		));
		Value projectRootObject = new ObjectLiteralInstance(SourceFileRange.EMPTY_SET, 
				TreeMap.treeMap(Ord.stringOrd, rootSlots));
		this.rootObject = runtime.extendedWith(projectRootObject);
	}

	public static Environment forSourceFile(Path sourceFilePath) {
		return new Environment(sourceFilePath);
	}
	
	public static Environment forCurrentDirectory() {
		return forSourceFile(Paths.get("").toAbsolutePath());
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
	public ObservableValue<Environment> toObservableValue() {
		if(observable == null)
			observable = new ObservableEnvironment(this);
		return observable;
	}

}
