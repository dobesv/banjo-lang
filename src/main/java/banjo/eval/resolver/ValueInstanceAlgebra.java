package banjo.eval.resolver;

import banjo.eval.BaseSlotNotFound;
import banjo.eval.ExtendedObject;
import banjo.eval.expr.CallInstance;
import banjo.eval.expr.FunctionInstance;
import banjo.eval.expr.ObjectLiteralInstance;
import banjo.eval.expr.SlotInstance;
import banjo.expr.free.FreeExpression;
import banjo.expr.util.SourceFileRange;
import banjo.value.SlotValue;
import banjo.value.Value;
import banjo.value.fail.FailWithMessage;
import banjo.value.fail.FailWithSourceFileRangesAndMessage;
import banjo.value.fail.UnboundIdentifier;
import banjo.value.fail.UnboundSlotObject;
import banjo.value.fail.UnresolvedCodeError;
import banjo.value.kernel.KernelBooleanValue;
import banjo.value.kernel.KernelNumberValue;
import banjo.value.kernel.KernelStringValue;
import banjo.value.meta.ArgMapper;
import banjo.value.meta.DynamicCallProxy;
import banjo.value.meta.DynamicSlotProxy;
import banjo.value.meta.FunctionComposition;
import banjo.value.meta.SlotMapper;
import banjo.value.meta.SlotMemoizer;
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
    public Value call(List<Value> trace, Set<SourceFileRange> ranges, Value callee, List<Value> args) {
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
    public Value functionInstance(Set<SourceFileRange> ranges, List<String> args, FreeExpression body, Option<String> sourceObjectBinding,
        Value trait, TreeMap<NameRef, Value> closure) {
        return new FunctionInstance(ranges, args, body, sourceObjectBinding, trait, closure);
    }

    @Override
    public Value kernelNumber(Set<SourceFileRange> ranges, Number number, Value trueValue) {
        return new KernelNumberValue(number, trueValue);
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
    public Value slotValue(Value object, Value self, String slotName, Set<SourceFileRange> ranges, Value prevSlotValue) {
        return new SlotValue(object, self, slotName, ranges, prevSlotValue);
    }

    @Override
    public Value kernelString(Set<SourceFileRange> ranges, String text, Value trueValue) {
        return new KernelStringValue(text, trueValue);
    }

    @Override
    public Value kernelBoolean(Set<SourceFileRange> ranges, boolean value, Value trueValue) {
        return new KernelBooleanValue(value, trueValue);
    }

    @Override
    public Value badExpr(Set<SourceFileRange> ranges, String message) {
        return new UnresolvedCodeError(message, ranges);
    }

    @Override
    public Value baseSlotNotFound(List<Value> trace, String slotName, Set<SourceFileRange> ranges, Value object) {
        return new BaseSlotNotFound<Value>(trace, slotName, ranges, object);
    }

    @Override
    public Value unboundSlotSelfName(List<Value> trace, Set<SourceFileRange> ranges, String id) {
        return new UnboundSlotObject(trace, ranges, id);
    }

    @Override
    public Value argMapperFactory() {
        return Value.function(ArgMapper::new);
    }

    @Override
    public Value dynamicCallProxyFactory() {
        return Value.function(DynamicCallProxy::new);
    }

    @Override
    public Value dynamicSlotProxyFactory() {
        return Value.function(DynamicSlotProxy::new);
    }

    @Override
    public Value functionCompositionFunction() {
        return Value.function(FunctionComposition::new);
    }

    @Override
    public Value slotMapperFactory() {
        return Value.function(SlotMapper::new);
    }

    @Override
    public Value failFunction() {
        // TODO Capture the trace by creating a custom Value type and implement
        // call().
        return Value.function(message -> new FailWithMessage(List.nil(), message));
    }

    @Override
    public Value extendFunction() {
        return Value.function(ExtendedObject::new);
    }

    @Override
    public Value fail(Set<SourceFileRange> ranges, String message) {
        return new FailWithSourceFileRangesAndMessage(List.nil(), ranges, message);
    }

    @Override
    public Value unboundIdentifier(Set<SourceFileRange> ranges, String name) {
        return new UnboundIdentifier(List.nil(), ranges, name);
    }

}
