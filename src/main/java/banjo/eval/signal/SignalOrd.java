package banjo.eval.signal;

import banjo.expr.core.KernelGlobalObject;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.KernelStringLiteral;
import banjo.value.fail.Fail;
import fj.F2Functions;
import fj.Ord;
import fj.Ordering;
import fj.data.List;

public class SignalOrd {

    public static Ordering _cmp(Signal a, Signal b) {
        if (a == b)
            return Ordering.EQ;
        
        return a.acceptVisitor(new SignalVisitor<Ordering>() {

            @Override
            public Ordering call(CallSignal callSignal1) {
                return callSignal1.acceptVisitor(new SignalVisitor<Ordering>() {

                    @Override
                    public Ordering call(CallSignal callSignal2) {
                        return CallSignal.CALL_ORD.compare(callSignal1, callSignal2);
                    }

                    @Override
                    public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering extendedObject(ExtendedObjectSignal extendedObjectSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering slotMemoizer(SlotMemoizerSignal slotMemoizerSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteralSignal objectLiteralSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering slotValue(SlotValueSignal slotValueSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering failure(Fail fail) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering function(FunctionSignal functionSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering number(NumberLiteral n) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering string(KernelStringLiteral s) {
                        return Ordering.LT;
                    }
                });
            }

            @Override
            public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
                return kernelGlobalObject.acceptVisitor(new SignalVisitor<Ordering>() {

                    @Override
                    public Ordering call(CallSignal callSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject2) {
                        return Ord.intOrd.compare(kernelGlobalObject.ordinal(), kernelGlobalObject2.ordinal());
                    }

                    @Override
                    public Ordering extendedObject(ExtendedObjectSignal extendedObjectSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering slotMemoizer(SlotMemoizerSignal slotMemoizerSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteralSignal objectLiteralSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering slotValue(SlotValueSignal slotValueSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering failure(Fail fail) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering function(FunctionSignal functionSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering number(NumberLiteral n) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering string(KernelStringLiteral s) {
                        return Ordering.LT;
                    }
                });
            }

            @Override
            public Ordering extendedObject(ExtendedObjectSignal extendedObjectSignal1) {
                return extendedObjectSignal1.acceptVisitor(new SignalVisitor<Ordering>() {

                    @Override
                    public Ordering call(CallSignal callSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering extendedObject(ExtendedObjectSignal extendedObjectSignal2) {
                        return ExtendedObjectSignal.ORD.compare(extendedObjectSignal1, extendedObjectSignal2);
                    }

                    @Override
                    public Ordering slotMemoizer(SlotMemoizerSignal slotMemoizerSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteralSignal objectLiteralSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering slotValue(SlotValueSignal slotValueSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering failure(Fail fail) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering function(FunctionSignal functionSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering number(NumberLiteral n) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering string(KernelStringLiteral s) {
                        return Ordering.LT;
                    }
                });
            }

            @Override
            public Ordering slotMemoizer(SlotMemoizerSignal slotMemoizerSignal1) {
                return slotMemoizerSignal1.acceptVisitor(new SignalVisitor<Ordering>() {

                    @Override
                    public Ordering call(CallSignal callSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering extendedObject(ExtendedObjectSignal extendedObjectSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slotMemoizer(SlotMemoizerSignal slotMemoizerSignal2) {
                        return SlotMemoizerSignal.ORD.compare(slotMemoizerSignal1, slotMemoizerSignal2);
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteralSignal objectLiteralSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering slotValue(SlotValueSignal slotValueSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering failure(Fail fail) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering function(FunctionSignal functionSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering number(NumberLiteral n) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering string(KernelStringLiteral s) {
                        return Ordering.LT;
                    }
                });
            }

            @Override
            public Ordering objectLiteral(ObjectLiteralSignal objectLiteralSignal1) {
                return objectLiteralSignal1.acceptVisitor(new SignalVisitor<Ordering>() {

                    @Override
                    public Ordering call(CallSignal callSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering extendedObject(ExtendedObjectSignal extendedObjectSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slotMemoizer(SlotMemoizerSignal slotMemoizerSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteralSignal objectLiteralSignal2) {
                        return ObjectLiteralSignal.ORD.compare(objectLiteralSignal1, objectLiteralSignal2);
                    }

                    @Override
                    public Ordering slotValue(SlotValueSignal slotValueSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering failure(Fail fail) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering function(FunctionSignal functionSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering number(NumberLiteral n) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering string(KernelStringLiteral s) {
                        return Ordering.LT;
                    }
                });
            }

            @Override
            public Ordering slotValue(SlotValueSignal slotValueSignal) {
                return slotValueSignal.acceptVisitor(new SignalVisitor<Ordering>() {

                    @Override
                    public Ordering call(CallSignal callSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering extendedObject(ExtendedObjectSignal extendedObjectSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slotMemoizer(SlotMemoizerSignal slotMemoizerSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteralSignal objectLiteralSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slotValue(SlotValueSignal slotValueSignal2) {
                        return SlotValueSignal.ORD.compare(slotValueSignal, slotValueSignal2);
                    }

                    @Override
                    public Ordering failure(Fail fail) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering function(FunctionSignal functionSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering number(NumberLiteral n) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering string(KernelStringLiteral s) {
                        return Ordering.LT;
                    }
                });
            }

            @Override
            public Ordering failure(Fail fail) {
                return fail.acceptVisitor(new SignalVisitor<Ordering>() {

                    @Override
                    public Ordering call(CallSignal callSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering extendedObject(ExtendedObjectSignal extendedObjectSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slotMemoizer(SlotMemoizerSignal slotMemoizerSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteralSignal objectLiteralSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slotValue(SlotValueSignal slotValueSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering failure(Fail fail2) {
                        return Ord.stringOrd.compare(fail.toString(), fail2.toString());
                    }

                    @Override
                    public Ordering function(FunctionSignal functionSignal) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering number(NumberLiteral n) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering string(KernelStringLiteral s) {
                        return Ordering.LT;
                    }
                });
            }

            @Override
            public Ordering function(FunctionSignal functionSignal) {
                return functionSignal.acceptVisitor(new SignalVisitor<Ordering>() {

                    @Override
                    public Ordering call(CallSignal callSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering extendedObject(ExtendedObjectSignal extendedObjectSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slotMemoizer(SlotMemoizerSignal slotMemoizerSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteralSignal objectLiteralSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slotValue(SlotValueSignal slotValueSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering failure(Fail fail2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering function(FunctionSignal functionSignal2) {
                        return FunctionSignal.ORD.compare(functionSignal, functionSignal2);
                    }

                    @Override
                    public Ordering number(NumberLiteral n) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering string(KernelStringLiteral s) {
                        return Ordering.LT;
                    }
                });
            }

            @Override
            public Ordering number(NumberLiteral n) {
                return n.acceptVisitor(new SignalVisitor<Ordering>() {

                    @Override
                    public Ordering call(CallSignal callSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering extendedObject(ExtendedObjectSignal extendedObjectSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slotMemoizer(SlotMemoizerSignal slotMemoizerSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteralSignal objectLiteralSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slotValue(SlotValueSignal slotValueSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering failure(Fail fail2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering function(FunctionSignal functionSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering number(NumberLiteral n2) {
                        return NumberLiteral.ORD.compare(n, n2);
                    }

                    @Override
                    public Ordering string(KernelStringLiteral s) {
                        return Ordering.LT;
                    }
                });
            }

            @Override
            public Ordering string(KernelStringLiteral s) {
                return s.acceptVisitor(new SignalVisitor<Ordering>() {

                    @Override
                    public Ordering call(CallSignal callSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering extendedObject(ExtendedObjectSignal extendedObjectSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slotMemoizer(SlotMemoizerSignal slotMemoizerSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteralSignal objectLiteralSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering slotValue(SlotValueSignal slotValueSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering failure(Fail fail2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering function(FunctionSignal functionSignal2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering number(NumberLiteral n2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering string(KernelStringLiteral s2) {
                        return KernelStringLiteral.ORD.compare(s, s2);
                    }
                });
            }
        });
    }

    public static Ord<Signal> ORD = Ord.ord(F2Functions.curry(SignalOrd::_cmp));
    public static final Ord<List<Signal>> LIST_ORD = Ord.listOrd(SignalOrd.ORD);

}
