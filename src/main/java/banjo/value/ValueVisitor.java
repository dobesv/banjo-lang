package banjo.value;

import banjo.eval.ClosedObject;
import banjo.eval.ExtendedObject;
import banjo.eval.ExtendedObject.ChainedBaseFunctionImpl;
import banjo.eval.Fail;
import banjo.eval.expr.CallInstance;
import banjo.eval.expr.FunctionInstance;
import banjo.eval.expr.ObjectLiteralInstance;
import banjo.eval.util.LazyBoundValue;
import banjo.value.kernel.KernelBooleanValue;
import banjo.value.kernel.KernelListValue;
import banjo.value.kernel.KernelMapValue;
import banjo.value.kernel.KernelNumberValue;
import banjo.value.kernel.KernelStringValue;
import banjo.value.meta.ArgMapper;
import banjo.value.meta.DynamicCallProxy;
import banjo.value.meta.DynamicSlotProxy;
import banjo.value.meta.FunctionComposition;
import banjo.value.meta.SlotMapper;

public interface ValueVisitor<T> {

    /**
     * Primitive number
     */
    T kernelNumber(KernelNumberValue kernelNumberValue);

    /**
     * Primitive string
     */
    T kernelString(KernelStringValue kernelStringValue);

    /**
     * Primitive boolean
     */
    T kernelBoolean(KernelBooleanValue kernelBooleanValue);

    /**
     * Primitive function of two arguments
     */
    T kernelBiFunction(KernelBiFunctionValue javaBiFunctionValue);

    /**
     * Primitive function of one argument
     */
    T kernelFunction(KernelFunctionValue javaFunctionValue);

    /**
     * Primitive list of Value
     */
    T kernelList(KernelListValue kernelArrayValue);

    /**
     * Primitive map of String -> Value
     */
    T kernelMap(KernelMapValue kernelMapValue);

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
     * Lazy environment lookup
     */
    T lazyBoundValue(LazyBoundValue lazyBoundValue);

    /**
     * Lazy call thunk
     */
    T call(CallInstance callInstance);

    /**
     * Lazy method call. Having this separate from call is a slight
     * optimization.
     */
    T methodCall(MethodCallInstance methodCallInstance);

    /**
     * Lazy slot calculation
     */
    T slotValue(SlotValue slotValue);

    /**
     * Special meta-object that applies a function to each argument before
     * passing it along to a delegate function.
     */
    T argMapper(ArgMapper argMapper);

    /**
     * Special meta-object that intercepts a call and captures its arguments as
     * a function that passes those same arguments to its own argument. It then
     * passes that function to a provided delegate to do with as it pleases.
     */
    T dynamicCallProxy(DynamicCallProxy dynamicCallProxy);

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

}
