package banjo.value;

import banjo.eval.ClosedObject;
import banjo.eval.ExtendedObject;
import banjo.eval.ExtendedObject.ChainedBaseFunctionImpl;
import banjo.eval.Fail;
import banjo.eval.expr.FunctionInstance;
import banjo.eval.expr.ObjectLiteralInstance;
import banjo.value.kernel.KernelBiFunctionValue;
import banjo.value.kernel.KernelBooleanValue;
import banjo.value.kernel.KernelFunctionValue;
import banjo.value.kernel.KernelListValue;
import banjo.value.kernel.KernelMapValue;
import banjo.value.kernel.KernelNumberValue;
import banjo.value.kernel.KernelStringValue;
import banjo.value.meta.ArgMapper;
import banjo.value.meta.DynamicCallProxy;
import banjo.value.meta.DynamicSlotProxy;
import banjo.value.meta.FunctionComposition;
import banjo.value.meta.SlotMapper;

public abstract class BaseValueVisitor<T> implements ValueVisitor<T> {
    public abstract T fallback();

    @Override
    public T kernelNumber(KernelNumberValue kernelNumberValue) {
        return fallback();
    }

    @Override
    public T kernelBoolean(KernelBooleanValue kernelNumberValue) {
        return fallback();
    }

    @Override
    public T kernelString(KernelStringValue kernelStringValue) {
        return fallback();
    }

    @Override
    public T objectBaseValue(ObjectBaseValue objectBaseValue) {
        return fallback();
    }

    @Override
    public T failure(Fail fail) {
        return fallback();
    }

    @Override
    public T kernelBiFunction(KernelBiFunctionValue javaBiFunctionValue) {
        return fallback();
    }

    @Override
    public T kernelFunction(KernelFunctionValue javaFunctionValue) {
        return fallback();
    }

    @Override
    public T objectLiteralInstance(ObjectLiteralInstance objectLiteralInstance) {
        return fallback();
    }

    @Override
    public T kernelList(KernelListValue kernelArrayValue) {
        return fallback();
    }

    @Override
    public T kernelMap(KernelMapValue kernelMapValue) {
        return fallback();
    }

    @Override
    public T argMapper(ArgMapper argMapper) {
        return fallback();
    }

    @Override
    public T dynamicCallProxy(DynamicCallProxy dynamicCallProxy) {
        return fallback();
    }

    @Override
    public T dynamicSlotProxy(DynamicSlotProxy dynamicSlotProxy) {
        return fallback();
    }

    @Override
    public T functionComposition(FunctionComposition functionComposition) {
        return fallback();
    }

    @Override
    public T slotMapper(SlotMapper slotMapper) {
        return fallback();
    }

    @Override
    public T closedObject(ClosedObject closedObject) {
        return fallback();
    }

    @Override
    public T baseFunctionChain(ChainedBaseFunctionImpl chainedBaseFunctionImpl) {
        return fallback();
    }

    @Override
    public T extendedObject(ExtendedObject extendedObject) {
        return fallback();
    }

    @Override
    public T functionInstance(FunctionInstance functionInstance) {
        return fallback();
    }
}
