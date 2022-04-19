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

public abstract class BaseValueVisitor<T> implements ValueVisitor<T> {
    public abstract T fallback();

    @Override
    public T objectBaseValue(ObjectBaseValue objectBaseValue) {
        return fallback();
    }

    @Override
    public T failure(Fail fail) {
        return fallback();
    }

    @Override
    public T objectLiteralInstance(ObjectLiteralInstance objectLiteralInstance) {
        return fallback();
    }

    @Override
    public T argMapper(ArgMapper argMapper) {
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

    @Override
    public T kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
        return fallback();
    }

    @Override
    public T selector(Selector selector) {
        return fallback();
    }

    @Override
    public T tuple(TupleValue tupleValue) {
        return fallback();
    }

    @Override
    public T number(NumberLiteral numberLiteral) {
        return fallback();
    }

    @Override
    public T string(StringLiteral stringLiteral) {
        return fallback();
    }

    public abstract T calculatedValue(CalculatedValue v);

    @Override
    public T methodCall(MethodCallInstance methodCallInstance) {
        return calculatedValue(methodCallInstance);
    }

    @Override
    public T slotValue(SlotValue slotValue) {
        return calculatedValue(slotValue);
    }

    @Override
    public T lazyBoundValue(LazyBoundValue lazyBoundValue) {
        return calculatedValue(lazyBoundValue);
    }

    @Override
    public T projectRoot(ProjectRootValue projectRootValue) {
        return calculatedValue(projectRootValue);
    }

    @Override
    public T call(CallInstance callInstance) {
        return calculatedValue(callInstance);
    }
}
