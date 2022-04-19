package banjo.eval.signal;

import banjo.expr.core.KernelGlobalObject;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.KernelStringLiteral;
import banjo.value.fail.Fail;

public interface SignalVisitor<T> {

    T call(CallSignal callSignal);

    T kernelGlobalObject(KernelGlobalObject kernelGlobalObject);

    T extendedObject(ExtendedObjectSignal extendedObjectSignal);

    T slotMemoizer(SlotMemoizerSignal slotMemoizerSignal);

    T objectLiteral(ObjectLiteralSignal objectLiteralSignal);

    T slotValue(SlotValueSignal slotValueSignal);

    T failure(Fail fail);

    T function(FunctionSignal functionSignal);

    T number(NumberLiteral n);

    T string(KernelStringLiteral s);
}
