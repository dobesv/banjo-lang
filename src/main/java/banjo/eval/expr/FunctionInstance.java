package banjo.eval.expr;

import banjo.eval.EvalContext;
import banjo.eval.resolver.ClosureResolver;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.NameRefAlgebra;
import banjo.eval.resolver.ValueInstanceAlgebra;
import banjo.expr.free.FreeExpression;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import banjo.value.FunctionTrait;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import banjo.value.fail.ArgumentNotSupplied;
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
	private static final TreeMap<NameRef, Value> EMPTY_CLOSURE = TreeMap.<NameRef, Value>empty(NameRef.ORD);

    public static class ParameterResolver implements NameRefAlgebra<Option<Value>> {
        public final FunctionInstance f;
        public final TreeMap<String, Value> passedArgMap;
        public final Value baseCallable;

        public ParameterResolver(FunctionInstance f, Value baseCallable, TreeMap<String, Value> passedArgMap) {
            this.f = f;
            this.baseCallable = baseCallable;
            this.passedArgMap = passedArgMap;
        }

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
        public Option<Value> functionBase(Set<SourceFileRange> ranges, String calleeBindingName) {
            if (f.calleeBinding.exists(calleeBindingName::equals))
                return Option.some(baseCallable);
            return Option.none();
        }

        @Override
        public Option<Value> invalid(Set<SourceFileRange> ranges, String reason) {
            return Option.none();
        }

    }

    public final Set<SourceFileRange> ranges;
    public final List<String> args;
	public final FreeExpression body;
    public final Option<String> calleeBinding;
    public final TreeMap<NameRef, Value> closure;

    public FunctionInstance(Set<SourceFileRange> ranges, List<String> args,
        FreeExpression body, Option<String> calleeBinding, Value trait, TreeMap<NameRef, Value> closure) {
        super(trait);
		this.ranges = ranges;
		this.args = args;
		this.body = body;
		this.calleeBinding = calleeBinding;
        this.closure = closure;
    }

	@Override
    public Value call(EvalContext<Value> ctx, Value callee, Value baseCallable, List<Value> passedArgs) {
        List<Value> missingArgs = args.drop(passedArgs.length()).map(name -> new ArgumentNotSupplied(ctx, name));
        TreeMap<String, Value> passedArgMap = TreeMap.iterableTreeMap(Ord.stringOrd,
                args.zip(passedArgs.append(missingArgs)));
        EvalContext<Value> newCtx = ctx.cons(this);
        NameRefAlgebra<Option<Value>> paramResolver = new ParameterResolver(this, baseCallable, passedArgMap);
        return body.eval(newCtx, new ClosureResolver<Value>(closure, ValueInstanceAlgebra.INSTANCE, paramResolver),
                ValueInstanceAlgebra.INSTANCE);
	}

	@Override
    public String toStringFallback(EvalContext<Value> ctx) {
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
    public Value callMethod(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
        return trait.callMethod(ctx, name, ranges, targetObject, fallback, args);
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
    @Override
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

    public static FunctionInstance identityFunction(Value functionTrait) {
        return new FunctionInstance(SourceFileRange.EMPTY_SET, List.single(Identifier.ARG_1.id), Identifier.ARG_1,
                Option.none(), functionTrait, EMPTY_CLOSURE);
    }

    /**
     * Return a function that ignores its arguments and always returns the same
     * value.
     * 
     * @param valueToReturn
     * @param functionTrait
     * @return
     */
    public static FunctionInstance constantFunction(Value valueToReturn, Value functionTrait) {
        return new FunctionInstance(SourceFileRange.EMPTY_SET, List.nil(), Identifier.ARG_1, Option.none(),
                functionTrait, EMPTY_CLOSURE.set(Identifier.ARG_1, valueToReturn));
    }
}
