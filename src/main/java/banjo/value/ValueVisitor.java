package banjo.value;

import banjo.eval.ClosedObject;
import banjo.eval.ExtendedObject;
import banjo.eval.ExtendedObject.ChainedBaseFunctionImpl;
import banjo.eval.expr.CallInstance;
import banjo.eval.expr.FunctionInstance;
import banjo.eval.expr.ObjectLiteralInstance;
import banjo.eval.util.LazyBoundValue;
import banjo.eval.util.Selector;
import banjo.expr.core.KernelGlobalObject;
import banjo.expr.free.ProjectRootValue;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;
import banjo.value.fail.Fail;
import banjo.value.special.ArgMapper;
import banjo.value.special.DynamicSlotProxy;
import banjo.value.special.FunctionComposition;
import banjo.value.special.SlotMapper;

public interface ValueVisitor<T> {
    /**
     * Some kind of critical programmer error
     */
    T failure(Fail fail);

    /**
     * Object whose job it is to implement base slot lookup.
     */
    T objectBaseValue(ObjectBaseValue objectBaseValue);

    /**
     * Object literal bound in an environment
     */
    T objectLiteralInstance(ObjectLiteralInstance objectLiteralInstance);

    /**
     * Special meta-object that applies a function to each argument before
     * passing it along to a delegate function.
     */
    T argMapper(ArgMapper argMapper);

    /**
     * Catch any slot access to the object and pass it as a selector to a
     * provided delegate function.
     */
    T dynamicSlotProxy(DynamicSlotProxy dynamicSlotProxy);

    /**
     * Function composition that works with any number of parameters.
     */
    T functionComposition(FunctionComposition functionComposition);

    /**
     * Wrap an object with mapper that lazily applies some function to every
     * slot.
     * 
     * Useful in combination with a projection to select out only the slots that
     * actually need to be mapped.
     */
    T slotMapper(SlotMapper slotMapper);

    /**
     * Wrapper that closes an object for extension. Once closed, the object
     * cannot "see" any slots in any extension it has, or if used as an
     * extension it cannot see slots in the base. Note that the slots in the
     * object can still be replaced using extension, but if a slot calculation
     * is ever delegated to the object these replacements won't affect the
     * result.
     */
    T closedObject(ClosedObject closedObject);

    /**
     * User defined function (closure)
     */
    T functionInstance(FunctionInstance functionInstance);

    /**
     * Object extension result
     */
    T extendedObject(ExtendedObject extendedObject);

    /**
     * When an extended object is called, the function has the option of
     * attempting to call its "previous" implementation. If the function is
     * extended twice, we need to keep track not only of the previous
     * implementation, but the implementation prior. This type of value tracks
     * those earlier implementations and provides them to the function when it
     * is called.
     */
    T baseFunctionChain(ChainedBaseFunctionImpl chainedBaseFunctionImpl);

    T kernelGlobalObject(KernelGlobalObject kernelGlobalObject);

    T tuple(TupleValue tupleValue);

    T selector(Selector selector);

    T number(NumberLiteral numberLiteral);

    T string(StringLiteral stringLiteral);

    T methodCall(MethodCallInstance methodCallInstance);

    T slotValue(SlotValue slotValue);

    T projectRoot(ProjectRootValue projectRootValue);

    T lazyBoundValue(LazyBoundValue lazyBoundValue);

    T call(CallInstance callInstance);

}
