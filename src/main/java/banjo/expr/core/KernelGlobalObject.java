package banjo.expr.core;

import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExpr;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;

/**
 * An expression that references a kernel-provided special object. Many kernel
 * objects provide functionality that cannot be implemented from within the
 * language.
 * 
 * Others are simply going to be faster / simpler than an implementation within
 * the language.
 * 
 * It's better to avoid having too many kernel global objects as these objects
 * must be known about by the various components of the system - type checkers,
 * optimizers, and so on generally will have to treat these objects (and often
 * the objects that contain them or are produced by them) specially.
 */
public enum KernelGlobalObject implements CoreExpr {
    /** Add two numbers as 32-bit integers */
    INT32_SUM,
    /** Add two numbers as 64-bit integers */
    INT64_SUM,
    /** Add two numbers as arbitrary sized integers */
    INTEGER_SUM,
    /** Add two numbers as 32-bit floating point numbers */
    FLOAT32_SUM,
    /** Add two numbers as 64-bit floating point numbers */
    FLOAT64_SUM,
    /** Add two numbers as arbitrary sized decimals */
    DECIMAL_SUM,
    /** Add two numbers using whatever type can handle it */
    GENERAL_SUM,
    /** Subtract two numbers as 32-bit integers */
    INT32_DIFFERENCE,
    /** Subtract two numbers as 64-bit integers */
    INT64_DIFFERENCE,
    /** Subtract two numbers as arbitrary sized integers */
    INTEGER_DIFFERENCE,
    /** Subtract two numbers as 32-bit floating point numbers */
    FLOAT32_DIFFERENCE,
    /** Subtract two numbers as 64-bit floating point numbers */
    FLOAT64_DIFFERENCE,
    /** Subtract two numbers as arbitrary sized decimals */
    DECIMAL_DIFFERENCE,
    /** Add two numbers using whatever type can handle it */
    GENERAL_DIFFERENCE,
    /** Multiply two numbers as 32-bit integers */
    INT32_PRODUCT,
    /** Multiply two numbers as 64-bit integers */
    INT64_PRODUCT,
    /** Multiply two numbers as arbitrary sized integers */
    INTEGER_PRODUCT,
    /** Multiply two numbers as 32-bit floating point numbers */
    FLOAT32_PRODUCT,
    /** Multiply two numbers as 64-bit floating point numbers */
    FLOAT64_PRODUCT,
    /** Multiply two numbers as 64-bit floating point numbers */
    DECIMAL_PRODUCT,
    /** Multiply two number */
    GENERAL_PRODUCT,
    /** Divide two numbers as 32-bit integers */
    INT32_QUOTIENT,
    /** Divide two numbers as 64-bit integers */
    INT64_QUOTIENT,
    /** Divide two numbers as arbitrary sized integers */
    INTEGER_QUOTIENT,
    /** Divide two numbers as 32-bit floating point numbers */
    FLOAT32_QUOTIENT,
    /** Divide two numbers as 64-bit floating point numbers */
    FLOAT64_QUOTIENT,
    /** Divide two numbers as 64-bit floating point numbers */
    DECIMAL_QUOTIENT,
    /** Divide two numbers */
    GENERAL_QUOTIENT,
    /** Return the remainder after integer division. */
    GENERAL_REMAINDER,
    /**
     * Get the integer division remainder of two numbers as 32-bit integers
     */
    INT32_MODULO,
    /**
     * Get the integer division remainder of two numbers as 64-bit integers
     */
    INT64_MODULO,
    /**
     * Get the integer division remainder of two numbers as arbitrary sized integers
     */
    INTEGER_MODULO,

    /** General numeric equality */
    GENERAL_EQUAL,

    SLOT_MAPPER_FACTORY,
    ARG_MAPPER_FACTORY,
    DYNAMIC_SLOT_PROXY_FACTORY,
    EXTEND_FUNCTION,
    FAIL_FUNCTION,

    /**
     * Takes two functions f and g, and return a new function fg such that fg(...)
     * == f(g(...)). The special trick with this is that it works for any number of
     * arguments to g.
     */
    FUNCTION_COMPOSITION_FUNCTION,

    /**
     * Returns a function that passes the parameters to TUPLE_FACTORY to any
     * function passed to it.
     * 
     * Basically a way to carry around some values and pass them to a function.
     */
    TUPLE_FACTORY,

    /**
     * Global handle to all events, unfiltered. This is then processed using the
     * other event list operations defined below.
     */
    EVENTS_STREAM,

    /**
     * EVENTS_HEAD(stream, fallback)
     * 
     * Get the first event value in the stream as a signal. If the stream is empty,
     * the fallback provided is used as the signal value instead.
     */
    EVENTS_HEAD,

    /**
     * EVENTS_TAIL(stream)
     * 
     * Get the stream of events beyond the first event in the stream as a signal. If
     * there are fewer than two events left in the stream at this time, the tail
     * will be empty.
     */
    EVENTS_TAIL,

    /**
     * Global handle to the application clock as a signal. This is a 64-bit integer
     * number of ticks. The tick rate of the application depends on its
     * configuration.
     */
    APPLICATION_CLOCK,

    /**
     * Global handle to the system clock with millisecond resolution.
     */
    SYSTEM_CLOCK_MS,

    /**
     * Special object that is equal to the value of "language kernel number" in the
     * project root/global scope. Used to construct numbers for number literals in
     * any scope, even a projection.
     */
    NUMBER_FACTORY,

    /**
     * Special object that is equal to the value of "language kernel string" in the
     * project root/global scope. Used to construct strings for string literals in
     * any scope, even a projection.
     */
    STRING_FACTORY,

    /**
     * Special object that is equal to the value of "empty list" in the project
     * root/global scope. Used to construct lists for list literals in any scope,
     * even a projection.
     */
    EMPTY_LIST,

    /**
     * Special object that is equal to the value of "single element list" in the
     * project root/global scope. Used to construct lists for list literals in any
     * scope, even a projection.
     */
    LIST_ELEMENT_FACTORY,

    /**
     * Special object that is equal to the value of "function trait" in the project
     * root/global scope. Used to construct functions for function literals in any
     * scope, even a projection.
     */
    FUNCTION_TRAIT,
    GENERAL_ORDERING,
    GENERAL_ASCENDING,
    GENERAL_DESCENDING,
    PROJECT_ROOT,
    STRING_EQUALS,
    CURRENT_SCOPE,

    /**
     * Built-in function to apply a precondition to an expression. Takes a condition
     * expression which must be true (or truthy); if it is not, the function caller
     * is in violation of the contract.
     *
     * Because a precondition is considered a programmer error, evaluation will not
     * proceed if a precondition fails. The development tools may be able to use
     * some data flow analysis to warn the programmer about a possible precondition
     * failure.
     */
    APPLY_PRECONDITION,

    /**
     * Built-in function to apply a postcondition to an expression. Takes a
     * predicate and an expression. The expression is passed to the predicate; the
     * predicate must return true, evaluation will not proceed. The error indicates
     * a failure of the algorithm internal to the expression, or a failure in the
     * predicate.
     */
    APPLY_POSTCONDITION

    ;

    @Override
    public void toSource(StringBuffer sb) {
        sb.append(this.name());
    }

    @Override
    public Precedence getPrecedence() { return Precedence.ATOM; }

    @Override
    public Set<SourceFileRange> getRanges() { return SourceFileRange.EMPTY_SET; }

    @Override
    public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
        return visitor.kernelGlobalObject(this);
    }

    @Override
    public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
        return visitor.kernelGlobalObject(this);
    }

    // @Override
    // public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
    // return visitor.kernelGlobalObject(this);
    // }
    //
    // @Override
    // public Set<NameRef> getFreeRefs() {
    // return NameRef.EMPTY_SET;
    // }
    //
    // @Override
    // public boolean hasFreeRefs() {
    // return false;
    // }
    //
    // @Override
    // public Option<FreeExpression> partial(PartialResolver resolver) {
    // return Option.none();
    // }
    //
    // @Override
    // public <T> T eval(EvalContext<T> ctx, Resolver<T> resolver,
    // InstanceAlgebra<T> algebra) {
    // switch (this) {
    // case FUNCTION_TRAIT:
    // return algebra.slotValue(ctx.projectRoot, SourceFileRange.EMPTY_SET,
    // Identifier.FUNCTION_TRAIT.id);
    //
    // case EMPTY_LIST:
    // return algebra.slotValue(ctx.projectRoot, SourceFileRange.EMPTY_SET,
    // Identifier.EMPTY_LIST.id);
    //
    // case LIST_ELEMENT_FACTORY:
    // return algebra.slotValue(ctx.projectRoot, SourceFileRange.EMPTY_SET,
    // Identifier.SINGLE_ELEMENT_LIST.id);
    //
    // case NUMBER_FACTORY:
    // return algebra.slotValue(ctx.projectRoot, SourceFileRange.EMPTY_SET,
    // Identifier.LANGUAGE_KERNEL_NUMBER.id);
    //
    // case STRING_FACTORY:
    // return algebra.slotValue(ctx.projectRoot, SourceFileRange.EMPTY_SET,
    // Identifier.LANGUAGE_KERNEL_STRING.id);
    //
    // default:
    // return algebra.kernelGlobalObject(this);
    // }
    // }
    //
    // @Override
    // public <T> T acceptVisitor(SignalVisitor<T> visitor) {
    // return visitor.kernelGlobalObject(this);
    // }
    //
    // @Override
    // public <T> T acceptVisitor(ValueVisitor<T> visitor) {
    // return visitor.kernelGlobalObject(this);
    // }
    //
    // @Override
    // public Value call(EvalContext<Value> ctx, List<Value> arguments) {
    // switch (this) {
    // case EXTEND_FUNCTION:
    // if(arguments.isEmpty() || arguments.tail().isEmpty())
    // return new FailWithMessage(ctx, "Not enough arguments for " + this);
    // return new ExtendedObject(arguments.head(), arguments.tail().head());
    //
    // case SLOT_MAPPER_FACTORY:
    // if(arguments.isEmpty() || arguments.tail().isEmpty())
    // return new FailWithMessage(ctx, "Not enough arguments for " + this);
    // return new SlotMapper(arguments.head(), arguments.tail().head());
    //
    // case FUNCTION_COMPOSITION_FUNCTION:
    // if (arguments.isEmpty() || arguments.tail().isEmpty() ||
    // arguments.tail().tail().isEmpty())
    // return new FailWithMessage(ctx, "Not enough arguments for " + this);
    // Value f = arguments.head();
    // Value g = arguments.tail().head();
    // Value functionTrait = arguments.tail().tail().head();
    // return new FunctionComposition(f, g, functionTrait);
    //
    // case DYNAMIC_SLOT_PROXY_FACTORY:
    // if(arguments.isEmpty())
    // return new FailWithMessage(ctx, "Missing argument to " + this);
    // return new DynamicSlotProxy(arguments.head());
    //
    // case ARG_MAPPER_FACTORY:
    // if (arguments.isEmpty() || arguments.tail().isEmpty())
    // return new FailWithMessage(ctx, "Not enough arguments for " + this);
    // return new ArgMapper(arguments.head(), arguments.tail().head());
    //
    // case TUPLE_FACTORY:
    // return new TupleValue(arguments);
    //
    // case FAIL_FUNCTION:
    // if (arguments.isEmpty())
    // return new Fail(ctx);
    // return new FailWithMessage(ctx, arguments.head());
    //
    // case FUNCTION_TRAIT:
    // return ctx.projectRoot.callMethod(ctx, Identifier.FUNCTION_TRAIT.id,
    // SourceFileRange.EMPTY_SET, arguments);
    //
    // case EMPTY_LIST:
    // return ctx.projectRoot.callMethod(ctx, Identifier.EMPTY_LIST.id,
    // SourceFileRange.EMPTY_SET, arguments);
    //
    // case LIST_ELEMENT_FACTORY:
    // return ctx.projectRoot.callMethod(ctx, Identifier.SINGLE_ELEMENT_LIST.id,
    // SourceFileRange.EMPTY_SET,
    // arguments);
    //
    // case NUMBER_FACTORY:
    // return ctx.projectRoot.callMethod(ctx,
    // Identifier.LANGUAGE_KERNEL_NUMBER.id, SourceFileRange.EMPTY_SET,
    // arguments);
    //
    // case STRING_FACTORY:
    // return ctx.projectRoot.callMethod(ctx,
    // Identifier.LANGUAGE_KERNEL_STRING.id, SourceFileRange.EMPTY_SET,
    // arguments);
    //
    // default:
    // return new FailWithMessage(ctx, "Not callable: " + this);
    // }
    // }

    public final Identifier identifier = new Identifier(this.name());

    @Override
    public SourceExpr toSourceExpr() {
        return identifier;
    }
}
