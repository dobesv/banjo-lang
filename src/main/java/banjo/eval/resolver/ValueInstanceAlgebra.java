package banjo.eval.resolver;

import banjo.eval.BaseSlotNotFound;
import banjo.eval.EvalContext;
import banjo.eval.ExtendedObject;
import banjo.eval.expr.CallInstance;
import banjo.eval.expr.FunctionInstance;
import banjo.eval.expr.ObjectLiteralInstance;
import banjo.eval.expr.SlotInstance;
import banjo.expr.core.KernelGlobalObject;
import banjo.expr.free.FreeExpression;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.SourceFileRange;
import banjo.value.SlotValue;
import banjo.value.Value;
import banjo.value.fail.FailWithSourceFileRangesAndMessage;
import banjo.value.fail.UnboundIdentifier;
import banjo.value.fail.UnboundSlotObject;
import banjo.value.fail.UnresolvedCodeError;
import banjo.value.special.SlotMemoizer;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

/**
 * Helpful base class for implementation of Resolver<Value>, it provides
 * defaults for all the methods so an implementation only needs to fill in the
 * ones that are relevant.
 */
public class ValueInstanceAlgebra implements InstanceAlgebra<Value> {
    public static final ValueInstanceAlgebra INSTANCE = new ValueInstanceAlgebra();

    @Override
    public Value call(EvalContext<Value> ctx, Set<SourceFileRange> ranges, Value callee, List<Value> args) {
        return new CallInstance(ranges, callee, args);
    }

    @Override
    public Value extend(Value base, Value extension) {
        return new ExtendedObject(base, extension);
    }

    @Override
    public Value slotMemoizer(Value delegate) {
        return new SlotMemoizer(delegate);
    }

    @Override
    public Value functionInstance(Set<SourceFileRange> ranges, List<String> args, FreeExpression body, Option<String> calleeBinding,
        Value trait, TreeMap<NameRef, Value> closure) {
        return new FunctionInstance(ranges, args, body, calleeBinding, trait, closure);
    }

    @Override
    public Value objectLiteral(Set<SourceFileRange> ranges, TreeMap<String, SlotInstance<Value>> slots) {
        return new ObjectLiteralInstance(ranges, slots);
    }

    @Override
    public Value slotValue(Value object, Set<SourceFileRange> ranges, String slotName) {
        return new SlotValue(object, ranges, slotName);
    }

    @Override
    public Value slotValue(Value object, Value self, String slotName, Set<SourceFileRange> ranges,
            Option<Value> prevSlotValue) {
        return new SlotValue(object, self, slotName, ranges, prevSlotValue);
    }

    @Override
    public Value badExpr(Set<SourceFileRange> ranges, String message) {
        return new UnresolvedCodeError(message, ranges);
    }

    @Override
    public Value baseSlotNotFound(EvalContext<Value> ctx, String slotName, Set<SourceFileRange> ranges, Value object) {
        return new BaseSlotNotFound<Value>(ctx, slotName, ranges, object);
    }

    @Override
    public Value unboundSlotSelfName(EvalContext<Value> ctx, Set<SourceFileRange> ranges, String id) {
        return new UnboundSlotObject<Value>(ctx, ranges, id);
    }

    @Override
    public Value fail(Set<SourceFileRange> ranges, String message) {
        return new FailWithSourceFileRangesAndMessage(EvalContext.NONE, ranges, message);
    }

    @Override
    public Value unboundIdentifier(Set<SourceFileRange> ranges, String name) {
        return new UnboundIdentifier(EvalContext.NONE, ranges, name);
    }

    @Override
    public Value kernelGlobalObject(KernelGlobalObject ko) {
        return ko;
    }

    @Override
    public Value number(NumberLiteral n) {
        return n;
    }

    @Override
    public Value string(StringLiteral s) {
        return s;
    }
}
