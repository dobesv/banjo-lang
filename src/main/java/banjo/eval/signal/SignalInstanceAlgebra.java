package banjo.eval.signal;

import banjo.eval.BaseSlotNotFound;
import banjo.eval.EvalContext;
import banjo.eval.expr.SlotInstance;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.expr.core.KernelGlobalObject;
import banjo.expr.free.FreeExpression;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.KernelStringLiteral;
import banjo.expr.util.SourceFileRange;
import banjo.value.fail.FailWithMessage;
import banjo.value.fail.UnboundIdentifier;
import banjo.value.fail.UnboundSlotObject;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public class SignalInstanceAlgebra implements InstanceAlgebra<Signal> {

    @Override
    public Signal call(EvalContext<Signal> ctx, Set<SourceFileRange> ranges, Signal callee, List<Signal> args) {
        return new CallSignal(callee, args);
    }

    @Override
    public Signal extend(Signal base, Signal extension) {
        return new ExtendedObjectSignal(base, extension);
    }

    @Override
    public Signal slotMemoizer(Signal s) {
        return new SlotMemoizerSignal(s);
    }

    @Override
    public Signal objectLiteral(Set<SourceFileRange> ranges, TreeMap<String, SlotInstance<Signal>> slots) {
        return new ObjectLiteralSignal(ranges, slots);
    }

    @Override
    public Signal slotValue(Signal object, Set<SourceFileRange> ranges, String name) {
        return new SlotValueSignal(object, ranges, name);
    }

    @Override
    public Signal fail(Set<SourceFileRange> ranges, String message) {
        return new FailWithMessage(EvalContext.NONE, message);
    }

    @Override
    public Signal slotValue(Signal object, Signal originalObject, String slotName, Set<SourceFileRange> ranges,
            Option<Signal> prevSlotValue) {
        return new SlotValueSignal(object, originalObject, slotName, ranges, prevSlotValue);
    }

    @Override
    public Signal baseSlotNotFound(EvalContext<Signal> ctx, String slotName, Set<SourceFileRange> ranges, Signal object) {
        return new BaseSlotNotFound<Signal>(ctx, slotName, ranges, object);
    }

    @Override
    public Signal unboundSlotSelfName(EvalContext<Signal> ctx, Set<SourceFileRange> ranges, String id) {
        return new UnboundSlotObject<Signal>(ctx, ranges, id);
    }

    @Override
    public Signal functionInstance(Set<SourceFileRange> ranges, List<String> args, FreeExpression body,
            Option<String> calleeBinding, Signal trait, TreeMap<NameRef, Signal> closure) {
        return new FunctionSignal(ranges, args, body, calleeBinding, trait, closure);
    }

    @Override
    public Signal unboundIdentifier(Set<SourceFileRange> ranges, String name) {
        return new UnboundIdentifier(EvalContext.NONE, ranges, name);
    }

    @Override
    public Signal badExpr(Set<SourceFileRange> ranges, String message) {
        return new FailWithMessage(EvalContext.NONE, message);
    }


    @Override
    public Signal kernelGlobalObject(KernelGlobalObject ko) {
        return ko;
    }

    @Override
    public Signal number(NumberLiteral n) {
        return n;
    }

    @Override
    public Signal string(KernelStringLiteral s) {
        return s;
    }
}
