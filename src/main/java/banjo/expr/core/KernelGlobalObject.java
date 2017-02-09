package banjo.expr.core;

import banjo.eval.resolver.GlobalRef;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.Resolver;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionVisitor;
import banjo.expr.free.PartialResolver;
import banjo.expr.source.Precedence;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Option;
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
public enum KernelGlobalObject implements CoreExpr, FreeExpression {
    SLOT_MAPPER_FACTORY,
    ARG_MAPPER_FACTORY,
    DYNAMIC_CALL_PROXY_FACTORY,
    DYNAMIC_SLOT_PROXY_FACTORY,
    EXTEND_FUNCTION,
    FAIL_FUNCTION,
    FUNCTION_COMPOSITION_FUNCTION,
    REACTIVE_VALUE,
    KERNEL_TRUE,
    KERNEL_FALSE,
    CHAINED_BASE_CALLABLE,

    /**
     * Returns a function that passes the parameters to TUPLE_FACTORY to any
     * function passed to it.
     * 
     * Basically a way to carry around some values and pass them to a function.
     */
    TUPLE_FACTORY,

    /**
     * When a function has a "self-name" it will be bound to one of these with
     * two arguments - the function that was called to get into this body (might
     * not be the same function if this function was extended) and the base
     * function implementation if this function is used to extend another
     * function.
     * 
     * For example, given:
     * 
     * f(x) = f g(y) = g fg = f << g
     * 
     * In a call f(x), "f" is bound to FUNCTION_CALLEE_REFERENCE(f, {}). In a
     * call g(x), "g" is bound to FUNCTION_CALLEE_REFERENCE(g, {}). In a call
     * fg(x), "g" is bound to FUNCTION_CALLEE_REFERENCE(fg, f) inside the body
     * of g. If "g" called the base implementation of "f" then "f" would be
     * bound to FUNCTION_CALLEE_REFERENCE(fg, {}).
     */
    FUNCTION_CALLEE_REFERENCE,

    /**
     * When a slot body is calculated it is given a reference to the original
     * object the slot is being read from (after considering extension), plus
     * the (lazy) value of any slot in the "base" object if the object has
     * another definition of the same slot earlier in the extension chain of the
     * object and the slot's name as a kernel string.
     */
    SLOT_OBJECT_REFERENCE,

    /**
     * A suspension of a call - takes callable, callee, baseCallable, and the
     * actual arguments are the remainder of the call. Used as part of partial
     * evaluation to leave a call intact.
     */
    APPLY_FACTORY,

    /**
     * This function takes an object, a slot name (as a kernel string), and the
     * fallback slot value and evaluates this combination lazily.
     * 
     * A normal slot reference will "forget" the original base object slot value
     * fallback, so we use this special slot reference with a fallback function
     * when we need to preserve the fallback value.
     */
    READ_SLOT_WITH_FALLBACK,

    /**
     * References the value of an expression "[]" in any scope.
     * 
     * This is a scope-independent lookup of the empty list in the root of the
     * project.
     */
    EMPTY_LIST,

    /**
     * A function "x -> [x]" that works in any lexical scope.
     * 
     * This is a scope-independent lookup of the single element list factory in
     * the root of the project.
     */
    SINGLE_ELEMENT_LIST_FACTORY,

    /**
     * A function that wraps a "kernel string" into an object suitable for use
     * in the language. This is needed because the kernel string is
     * intentionally limited in its functionality; the library can thus wrap it
     * up with all the bells and whistles.
     */
    LANGUAGE_KERNEL_STRING_FACTORY,

    /**
     * A function that wraps a "kernel number" into an object suitable for use
     * in the language. This is needed because the kernel string is
     * intentionally limited in its functionality; the library can thus wrap it
     * up with all the bells and whistles.
     */
    LANGUAGE_KERNEL_NUMBER_FACTORY;

    @Override
    public void toSource(StringBuffer sb) {
        sb.append(this.name());
    }

    @Override
    public Precedence getPrecedence() {
        return Precedence.ATOM;
    }

    @Override
    public Set<SourceFileRange> getRanges() {
        return SourceFileRange.EMPTY_SET;
    }

    @Override
    public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
        return visitor.kernelGlobalObject(this);
    }

    @Override
    public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
        return visitor.kernelGlobalObject(this);
    }

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.kernelGlobalObject(this);
    }

    @Override
    public Set<NameRef> getFreeRefs() {
        return NameRef.EMPTY_SET;
    }

    @Override
    public boolean hasFreeRefs() {
        return false;
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver resolver) {
        return Option.none();
    }

    @Override
    public <T> T eval(List<T> trace, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        switch(this) {
        case APPLY_FACTORY:
            return algebra.applyFactory();
        case ARG_MAPPER_FACTORY:
            return algebra.argMapperFactory();
        case CHAINED_BASE_CALLABLE:
            return algebra.chainedBaseCallableFactory();
        case DYNAMIC_CALL_PROXY_FACTORY:
            return algebra.dynamicCallProxyFactory();
        case DYNAMIC_SLOT_PROXY_FACTORY:
            return algebra.dynamicSlotProxyFactory();
        case EXTEND_FUNCTION:
            return algebra.extendFunction();
        case FAIL_FUNCTION:
            return algebra.failFunction();
        case FUNCTION_CALLEE_REFERENCE:
            return algebra.functionCalleeReferenceFactory();
        case FUNCTION_COMPOSITION_FUNCTION:
            return algebra.functionCompositionFunction();
        case KERNEL_FALSE:
            return algebra.kernelBoolean(SourceFileRange.EMPTY_SET, false, resolver.global(GlobalRef.TRUE));
        case KERNEL_TRUE:
            return algebra.kernelBoolean(SourceFileRange.EMPTY_SET, true, resolver.global(GlobalRef.TRUE));
        case REACTIVE_VALUE:
            return algebra.reactiveValueFactory();
        case READ_SLOT_WITH_FALLBACK:
            return algebra.slotRefWithFallbackFactory();
        case SLOT_MAPPER_FACTORY:
            return algebra.slotMapperFactory();
        case SLOT_OBJECT_REFERENCE:
            return algebra.slotObjectReferenceFactory();
        case TUPLE_FACTORY:
            return algebra.tupleFactory();
        default:
            throw new UnsupportedOperationException("Missing implemention for " + this + ".eval()");
        }
    }

}
