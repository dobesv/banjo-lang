package banjo.eval.environment;

import static java.util.Objects.requireNonNull;

import java.nio.file.Path;
import java.nio.file.Paths;

import banjo.eval.ArgumentNotSupplied;
import banjo.eval.Fail;
import banjo.eval.SlotNotFound;
import banjo.eval.UnboundIdentifier;
import banjo.eval.util.JavaLanguageRuntimeImpl;
import banjo.eval.util.LazyBoundValue;
import banjo.event.PastEvent;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprFactory;
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

/**
 * An environment is used to resolve names to values when evaluating
 * expressions.
 * <p>
 * Names are resolved first against any locally defined variables or
 * function/method parameters, from the innermost "let" or function argument
 * list outwards.
 * <p>
 * When resolving expressions life <code>foo.bar</code> the part after the dot
 * is evaluated in an environment where all the slots of <code>foo</code> are
 * used to put values to names in the expression <code>bar<bar> which could be a
 * more complex expression, such as one in ()'s, {}'s, or []'s, possibly
 * including local variable definitions.
 * <p>
 * When not inside a projection (i.e. to the right of the "." operator) the
 * "project root object" is the default projection, allowing access to global
 * variables defined in the project root from the project source search path.
 */
public class Environment implements Reactive<Environment> {
    public final Value projectRootObject;
	public final Value rootObject;
	public final TreeMap<String, Binding> bindings;
	public final boolean reactive;
	private ObservableEnvironment observable;

    /**
     * Construct an environment.
     * 
     * @param rootObject
     *            Object whose slots become the toplevel bindings for the
     *            environment
     * @param bindings
     *            Local parameter and variable binding for the environment
     * @param projectRootObject
     *            Root object of the whole project; this is needed by string and
     *            number literals to construct objects, among other things
     * 
     */
    public Environment(Value rootObject, TreeMap<String, Binding> bindings, Value projectRootObject) {
        this.bindings = requireNonNull(bindings);
        this.rootObject = requireNonNull(rootObject);
        this.reactive = rootObject.isReactive() || checkReactive(bindings);
        this.projectRootObject = requireNonNull(projectRootObject);
	}
	
    /**
     * Construct an environment with no local variables or parameters in it.
     * 
     * Used to resolve projections of an object.
     * 
     * @param rootObject
     *            Object whose slots become the toplevel bindings for the
     *            environment
     * @param bindings
     *            Local parameter and variable binding for the environment
     * @param projectRootObject
     *            Root object of the whole project; this is needed by string and
     *            number literals to construct objects, among other things
     */
    public Environment(Value rootObject, Value projectRootObject) {
        this(rootObject, TreeMap.empty(Ord.stringOrd), projectRootObject);
	}

    /**
     * Constructor for the root environment of the project. The root object of
     * the project evaluates the slots of the root object in the environment in
     * which those slots are made available, so we have to bind / construct that
     * root object lazily to avoid infinite recursion.
     * 
     * @param rootFreeExpr
     *            Function to make a value from this environment for the root
     *            object and projectRootObject of the environment
     * @param runtimeName
     *            Name of the runtime in the new environment
     * @param runtime
     *            Bound to the name given as <code>runtimeName</code>
     */
    public Environment(FreeExpression rootFreeExpr, String runtimeName, Value runtime) {
        this.bindings = TreeMap.<String, Binding> empty(Ord.stringOrd).set(runtimeName, Binding.let(runtime));
        this.rootObject = this.projectRootObject = bindLazy(rootFreeExpr);
        this.reactive = false;
    }

    /**
     * Return true if any binding value in this environment is reactive.
     */
	public boolean checkReactive(TreeMap<?, Binding> bindings) {
		boolean reactive = false;
        for(P2<?, Binding> p : bindings) {
			if(p._2().isReactive()) {
				reactive = true;
				break;
			}
		}
		return reactive;
	}
	
    /**
     * Construct an environment by adding new local name -> value bindings to an
     * existing environment.
     * <p>
     * The values are provided as a FreeExpression so that they can be bound
     * into the same environment, allowing recursive bindings.
     * 
     * @param letBindings
     *            New name -> value bindings
     * @param parentEnv
     *            Parent environment
     */
	public Environment(TreeMap<String, FreeExpression> letBindings, Environment parentEnv) {
		// The trick here is that we mustn't "force" or calculate any value in the let until after we've set bindings
		this.bindings = letBindings.map(this::bindLazy).map(Binding::let).union(parentEnv.bindings);
		this.rootObject = parentEnv.rootObject;
		this.reactive = parentEnv.isReactive() || checkReactive(bindings);
        this.projectRootObject = parentEnv.projectRootObject;
	}

	@Override
	public String toString() {
        return projectRootObject + ".(" + bindings + " ⇒ ...)";
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
        return new Environment(newRootObject, newBindings, projectRootObject);
	}
	
    /**
     * Evaluate the given AST / <code>CoreExpr</code> in this environment.
     * 
     * @param ast
     *            Expression to evaluate
     * @return Evaluation result
     */
	public Value eval(CoreExpr ast) {
		return eval(FreeExpressionFactory.apply(ast));
	}

    /**
     * Evaluate a <code>FreeExpression</code> in this environment.
     * 
     * @param fx
     *            Expression to evaluate
     * @return Evaluation result
     */
	public Value eval(final FreeExpression fx) {
	    return fx.apply(this);
    }
	
    /**
     * Create a value which will calculate itself lazily when slots of it are
     * accessed; this is used to help with mutually referential expressions in
     * an environment.
     * 
     * @param fe
     *            Expression to evaluate, when needed
     * @return Value "Thunk" that wraps the <code>FreeExpression</code> in a
     *         lazily-evaluating container
     */
    private Value bindLazy(FreeExpression fe) {
		return new LazyBoundValue(fe, this);
	}
	
    /**
     * Get the value associated with a name in this environment.
     * 
     * @param id
     *            Name to look up
     * @param ranges
     *            Source location(s) to blame in an error message if the
     *            identifier is not bound to a value in this environment
     * @return <code>Value</code> that was found, or a <code>Value</code>
     *         representing an error that the name is not bound in this
     *         environment
     */
    public Value getValue(String id, Set<SourceFileRange> ranges) {
        if(id.equals(Identifier.PROJECT_ROOT.id))
            return projectRootObject;
        Option<Binding> binding = bindings.get(id);
        if(binding.isSome()) {
            return binding.some().getValue();
        }

        return new SlotValue(rootObject, rootObject, id, ranges, unboundIdentifier(id, ranges));
	}

    /**
     * Get a value from the environment without any source file ranges.
     * 
     * @param id
     *            Name to look up
     * @return <code>Value</code> that was found, or a <code>Fail</code>
     *         representing an error that the name is not bound in this
     *         environment
     */
    public Value getValue(String id) {
        return getValue(id, SourceFileRange.EMPTY_SET);
    }
	
    /**
     * Construct a "fail" object to return if no value was found for a name.
     * 
     * @param id
     *            Name being looked up
     * @param ranges
     *            Source file ranges where that name comes from
     * @return A <code>Fail</code> value instance
     */
    private Fail unboundIdentifier(String id, Set<SourceFileRange> ranges) {
        if(this.rootObject == this.projectRootObject)
            return new UnboundIdentifier(id, ranges);
        return new SlotNotFound(id, ranges, rootObject);
    }

    /**
     * Return a new environment that extends this environment with some new
     * name/value pairs.
     * <p>
     * The values will be calculated lazily on demand, in the same environment
     * as is returned by this method. This means that binding values may refer
     * to each other in their definitions.
     * 
     * @param letBindings
     *            New bindings to add to this environment to make a new one
     * @return A new environment
     */
	public Environment let(List<P2<String, FreeExpression>> letBindings) {
		return let(TreeMap.treeMap(Ord.stringOrd, letBindings));
	}

    /**
     * Return a new environment that extends this environment with one new
     * name/value binding.
     *
     * @param name
     *            Name of the binding
     * @param valueFactory
     *            Means of constructing the value from the environment
     * @return A new environment
     */
    public Environment let1(String name, FreeExpression valueFactory) {
        return let(List.single(P.p(name, valueFactory)));
    }

    /**
     * Return a new environment that extends this environment with some new
     * name/value pairs.
     * 
     * @param letBindings
     *            New bindings to add to this environment to make a new one
     * @return A new environment
     */
	public Environment let(TreeMap<String, FreeExpression> letBindings) {
		return new Environment(letBindings, this);
	}

    /**
     * Construct a new environment that extends this environment with some new
     * bindings. This is used for "special" bindings - those used to resolve
     * previous values of slots or previous implementations of functions.
     * 
     * @param bindings
     *            New bindings to add
     * @return A new environment
     */
	public Environment bind(TreeMap<String, Binding> bindings) {
		return update(rootObject, bindings.union(this.bindings));
	}
	
    /**
     * Calculate the top level environment for a given source file by figuring
     * out its project root object and creating an environment based on that.
     * <p>
     * The project root object is calculated by combining the first parent
     * folder of the given path containing a folder named ".banjo" and the list
     * of paths in the system property "banjo.path".
     * 
     * @param sourceFilePath
     *            Source path to search for a project at
     * @return A new environment
     */
	public static Environment forSourcePath(Path sourceFilePath) {
        CoreExpr projectAst = CoreExprFactory.INSTANCE.loadProjectAstForSourcePath(sourceFilePath);
        FreeExpression environmentRoot = FreeExpressionFactory.apply(projectAst);
        Value runtime = Value.fromJava(new JavaLanguageRuntimeImpl());
        return new Environment(environmentRoot, Identifier.LANGUAGE_KERNEL.id, runtime);
	}

    /**
     * Calculate the top level environment based on the current directory.
     * <p>
     * The project root object is calculated by combining the first parent
     * folder of the current directory containing a folder named ".banjo" and
     * the list of paths in the system property "banjo.path".
     * 
     * @return A new environment
     */
	public static Environment forCurrentDirectory() {
        return forSourcePath(Paths.get(""));
	}

    /**
     * Construct a new environment by adding a single binding to this one.
     * <p>
     * This is normally used to construct "special" bindings, such as for
     * looking up the overridden value of a slot or the function self reference.
     * 
     * @param id
     *            New name to bind
     * @param binding
     *            A description of the binding.
     * @return A new environment
     */
	public Environment bind(String id, Binding binding) {
		return bind(TreeMap.<String,Binding>empty(Ord.stringOrd).set(id, binding));
	}

    /**
     * Construct a new environment for a function body.
     * 
     * @param formalArgs
     *            Declared arguments of the function when it was defined
     * @param providedArgs
     *            Actual argument values provided to the function as it is
     *            called
     * @param sourceObjectBinding
     *            Optional name the function would like to bind to "itself" for
     *            recursive calling
     * @param recurse
     *            Function value that should be used for a recursive call, if
     *            any
     * @param prevImpl
     *            Function value taht should be used if attempting to call the
     *            "previous" implementation
     * @return A new environment to use to evaluate the function body.
     */
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
