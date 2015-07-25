package banjo.eval;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Function;

import banjo.eval.expr.BindingInstance;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.eval.util.LazyBoundValue;
import banjo.event.Event;
import banjo.expr.core.CoreExpr;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;
import banjo.expr.token.Identifier;
import banjo.value.Reaction;
import banjo.value.Reactive;
import banjo.value.Value;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.TreeMap;

public class Environment implements Function<String, Option<BindingInstance>>, Reactive<Environment> {

	public final TreeMap<String, BindingInstance> bindings;
	public final boolean reactive;

	public Environment(TreeMap<String, BindingInstance> bindings) {
		this.bindings = bindings;
		this.reactive = checkReactive(bindings);
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
		this.reactive = checkReactive(bindings);
	}

	@Override
    public Option<BindingInstance> apply(String t) {
    	return bindings.get(t);
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
		return reactions.from(newSlots).map(this::update);
	}

	@Override
	public boolean isReactive() {
		return reactive;
	}
	
	public Environment update(TreeMap<String, BindingInstance> newBindings) {
		if(newBindings == bindings)
			return this;
		return new Environment(newBindings);
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

	public Environment append(Environment child) {
		return update(child.bindings.union(bindings));
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
	public static Environment javaRoot() {
		return single(JAVA_RUNTIME_ID, BindingInstance.let(Value.fromJava(JavaRuntimeSupport.class)));		
	}
	
	public static Environment single(String id, BindingInstance binding) {
		TreeMap<String, BindingInstance> b = TreeMap.<String,BindingInstance>empty(Ord.stringOrd).set(id, binding);
		return new Environment(b);
	}

	public Environment bind(List<P2<String, BindingInstance>> bindings) {
		return bind(TreeMap.<String,BindingInstance>treeMap(Ord.stringOrd, bindings));
	}
	
	public Environment bind(TreeMap<String, BindingInstance> bindings) {
		return update(bindings.union(this.bindings));
	}
	
	public static Environment forSourceFile(Path sourceFilePath) {
		return javaRoot().let(bindingsForPath(sourceFilePath));
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
