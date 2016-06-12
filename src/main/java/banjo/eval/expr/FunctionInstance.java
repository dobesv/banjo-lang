package banjo.eval.expr;

import banjo.eval.resolver.ClosureResolver;
import banjo.eval.resolver.GlobalRef;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.NameRefAlgebra;
import banjo.eval.resolver.ValueInstanceAlgebra;
import banjo.expr.free.FreeExpression;
import banjo.expr.source.Operator;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import banjo.value.FunctionTrait;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.Ord;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

/**
 * Instance of a function literal.
 * <p>
 * On construction, the body should already have all globals and enclosed
 * variables embedded/enclosed in it, this assumes only the function parameters
 * and callee need to be bound when calling the function.
 */
public class FunctionInstance extends FunctionTrait implements Value {
	public final Set<SourceFileRange> ranges;
    public final List<String> args;
	public final FreeExpression body;
    public final Option<String> calleeBinding;
    public final Value trait;
    public final TreeMap<NameRef, Value> closure;

    public FunctionInstance(Set<SourceFileRange> ranges, List<String> args,
        FreeExpression body, Option<String> sourceObjectBinding, Value trait, TreeMap<NameRef, Value> closure) {
		this.ranges = ranges;
		this.args = args;
		this.body = body;
		this.calleeBinding = sourceObjectBinding;
        this.trait = trait;
        this.closure = closure;
    }

	@Override
    public Value call(List<Value> trace, Value originalCallee, Value baseCallable, List<Value> passedArgs) {
	    TreeMap<String, Value> passedArgMap = TreeMap.treeMap(Ord.stringOrd, args.zip(passedArgs));
	    List<Value> newTrace = trace.cons(this);
        // TODO Need to bind args and self name!
        NameRefAlgebra<Option<Value>> paramResolver = new NameRefAlgebra<Option<Value>>() {
            @Override
            public Option<Value> local(Set<SourceFileRange> ranges, String name) {
                return passedArgMap.get(name);
            }

            @Override
            public Option<Value> slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
                return visit(object).map(obj -> ValueInstanceAlgebra.INSTANCE.slotValue(obj, ranges, slotName));
            }

            @Override
            public Option<Value> baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
                return Option.none();
            }

            @Override
            public Option<Value> functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
                if(calleeBinding.exists(functionSelfName::equals))
                    return Option.some(baseCallable);
                return Option.none();
            }

            @Override
            public Option<Value> invalid(Set<SourceFileRange> ranges, String reason) {
                return Option.none();
            }

            @Override
            public Option<Value> global(GlobalRef g) {
                return Option.none();
            }
        };
        return body.eval(newTrace, new ClosureResolver<Value>(closure, ValueInstanceAlgebra.INSTANCE, paramResolver), ValueInstanceAlgebra.INSTANCE);
	}

	@Override
    public String toStringFallback(List<Value> trace) {
		final Option<SourceFileRange> loc = ranges.toStream().toOption();
        Option<String> bindingName = calleeBinding;
		StringBuffer sb = new StringBuffer();
		sb.append("<function");
        bindingName.forEach(x -> Identifier.toSource(x, sb.append(" ")));
		loc.forEach(x -> sb.append(" from ").append(x));
		sb.append(">");
		return sb.toString();
    }

    @Override
    public Value slot(List<Value> trace, Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
        if("label".equals(name))
            return super.slot(trace, self, name, ranges, fallback);
        if(Operator.FUNCTION_COMPOSITION_LEFT.getOp().equals(name))
            return Value.function(this::compose);
        return trait.slot(trace, self, name, ranges, fallback);
    }

    @Override
    public Value callMethod(List<Value> trace, String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
        return trait.callMethod(trace, name, ranges, targetObject, fallback, args);
    }

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.functionInstance(this);
    }

    /**
     * Original source file ranges where this function was defined, if known.
     */
    public Set<SourceFileRange> getRanges() {
        return ranges;
    }

    /**
     * List of argument names
     */
    public List<String> getArgs() {
        return args;
    }

    /**
     * Function body. On creation it is assumed that all free references in the
     * body refer only to regular arguments or the "callee" implicit argument.
     */
    public FreeExpression getBody() {
        return body;
    }

    /**
     * Optional name binding for the "callee"; this is the object that was
     * originally called, ignoring any extend operation.
     */
    public Option<String> getCalleeBinding() {
        return calleeBinding;
    }

    /**
     * Function trait that supplies helper slots for this function. Typically
     * functions all have the same trait value within a project.
     */
    public Value getTrait() {
        return trait;
    }

    /**
     * Function closure - any values from outside the function besides the
     * arguments are bundled with the function here when the function is
     * instantiated.
     */
    public TreeMap<NameRef, Value> getClosure() {
        return closure;
    }
}
