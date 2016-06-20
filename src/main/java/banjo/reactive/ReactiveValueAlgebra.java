package banjo.reactive;

import banjo.eval.expr.ClosedSlotInstance;
import banjo.eval.expr.OpenSlotInstance;
import banjo.eval.expr.SlotInstance;
import banjo.eval.expr.SlotInstanceVisitor;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.ValueInstanceAlgebra;
import banjo.expr.free.FreeExpression;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public class ReactiveValueAlgebra implements InstanceAlgebra<Either<ReactiveValue, Value>> {
    public static final ReactiveValueAlgebra INSTANCE = new ReactiveValueAlgebra();
    final ValueInstanceAlgebra valueAlgebra = ValueInstanceAlgebra.INSTANCE;

    @Override
    public Either<ReactiveValue, Value> call(List<Either<ReactiveValue, Value>> trace, Set<SourceFileRange> ranges, Either<ReactiveValue, Value> callee,
        List<Either<ReactiveValue, Value>> args) {
        if(callee.isRight() && args.forall(Either::isRight)) {
            return Either.right(valueAlgebra.call(Either.rights(trace), ranges, callee.right().value(), args.map(a -> a.right().value())));
        }
        return Either.left(new ReactiveCall(callee, callee, null, args));
    }

    @Override
    public Either<ReactiveValue, Value> extend(Either<ReactiveValue, Value> base, Either<ReactiveValue, Value> extension) {
        if(base.isRight() && extension.isRight()) {
            return Either.right(valueAlgebra.extend(base.right().value(), extension.right().value()));
        }
        return Either.left(new ReactiveExtend(base, extension));
    }

    @Override
    public Either<ReactiveValue, Value> slotMemoizer(Either<ReactiveValue, Value> object) {
        if(object.isRight())
            return Either.right(valueAlgebra.slotMemoizer(object.right().value()));

        return object;
        // TODO
        // return Either.left(new ReactiveSlotMemoizer(object));
    }

    @Override
    public Either<ReactiveValue, Value> objectLiteral(Set<SourceFileRange> ranges, TreeMap<String, SlotInstance<Either<ReactiveValue, Value>>> slots) {
        if(slots.values().forall(si -> si.acceptVisitor(new SlotInstanceVisitor<Either<ReactiveValue, Value>, Boolean>() {

            @Override
            public Boolean closed(ClosedSlotInstance<Either<ReactiveValue, Value>> closedSlotInstance) {
                return closedSlotInstance.value.isRight();
            }

            @Override
            public Boolean open(OpenSlotInstance<Either<ReactiveValue, Value>> openSlotInstance) {
                return openSlotInstance.closure.values().forall(Either::isRight);
            }
            
        }))) {
            return Either.right(valueAlgebra.objectLiteral(ranges, slots.map(si -> si.acceptVisitor(new SlotInstanceVisitor<Either<ReactiveValue, Value>, SlotInstance<Value>>() {

                        @Override
                        public SlotInstance<Value> closed(ClosedSlotInstance<Either<ReactiveValue, Value>> closedSlotInstance) {
                            return new ClosedSlotInstance<Value>(closedSlotInstance.value.right().value());
                        }

                        @Override
                        public SlotInstance<Value> open(OpenSlotInstance<Either<ReactiveValue, Value>> openSlotInstance) {
                            return new OpenSlotInstance<Value>(openSlotInstance.name, openSlotInstance.slotObjectRef, openSlotInstance.body,
                                openSlotInstance.closure.map(e -> e.right().value()));
                        }

                    }))));
        }

        return Either.left(new ReactiveObjectLiteral(ranges, slots));
    }

    @Override
    public Either<ReactiveValue, Value> slotValue(Either<ReactiveValue, Value> object, Set<SourceFileRange> ranges, String name) {
        return object.bimap(rv -> new ReactiveSlotValue(rv, name, ranges), value -> valueAlgebra.slotValue(value, ranges, name));
    }

    @Override
    public Either<ReactiveValue, Value> fail(Set<SourceFileRange> ranges, String message) {
        return Either.right(valueAlgebra.fail(ranges, message));
    }

    @Override
    public Either<ReactiveValue, Value> slotValue(Either<ReactiveValue, Value> object, Either<ReactiveValue, Value> self, String slotName,
        Set<SourceFileRange> ranges, Either<ReactiveValue, Value> prevSlotValue) {
        if(object.isRight() && self.isRight() && (prevSlotValue == null || prevSlotValue.isRight())) {
            return Either.right(
                valueAlgebra
                    .slotValue(object.right().value(), self.right().value(), slotName, ranges, prevSlotValue == null ? null : prevSlotValue.right().value()));
        }
        return Either.left(new ReactiveSlotValue(object, self, slotName, ranges, prevSlotValue));
    }

    @Override
    public Either<ReactiveValue, Value> baseSlotNotFound(List<Either<ReactiveValue, Value>> trace, String slotName, Set<SourceFileRange> ranges,
        Either<ReactiveValue, Value> object) {
        // This may want to update if the object changes, one day
        return Either.right(valueAlgebra.baseSlotNotFound(Either.rights(trace), slotName, ranges, ReactiveValue.currentValue(Either.lefts(trace), object)));
    }

    @Override
    public Either<ReactiveValue, Value> unboundSlotSelfName(List<Either<ReactiveValue, Value>> trace, Set<SourceFileRange> ranges, String id) {
        return Either.right(valueAlgebra.unboundSlotSelfName(Either.rights(trace), ranges, id));
    }

    @Override
    public Either<ReactiveValue, Value> functionInstance(Set<SourceFileRange> ranges, List<String> args, FreeExpression body,
        Option<String> sourceObjectBinding, Either<ReactiveValue, Value> trait, TreeMap<NameRef, Either<ReactiveValue, Value>> closure) {
        if(closure.values().forall(Either::isRight)) {
            return Either
                .right(valueAlgebra.functionInstance(ranges, args, body, sourceObjectBinding, _const(ranges, trait), closure.map(e -> e.right().value())));
        }
        return Either.left(new ReactiveFunctionInstance(ranges, args, body, sourceObjectBinding, trait, closure));
    }

    private Value _const(Set<SourceFileRange> ranges, Either<ReactiveValue, Value> trait) {
        return trait.right().orValue(() -> valueAlgebra.badExpr(ranges, "Should not be reactive"));
    }

    @Override
    public Either<ReactiveValue, Value> argMapperFactory() {
        return Either.right(valueAlgebra.argMapperFactory());
    }

    @Override
    public Either<ReactiveValue, Value> dynamicCallProxyFactory() {
        return Either.right(valueAlgebra.dynamicCallProxyFactory());
    }

    @Override
    public Either<ReactiveValue, Value> dynamicSlotProxyFactory() {
        return Either.right(valueAlgebra.dynamicSlotProxyFactory());
    }

    @Override
    public Either<ReactiveValue, Value> functionCompositionFunction() {
        return Either.right(valueAlgebra.functionCompositionFunction());
    }

    @Override
    public Either<ReactiveValue, Value> slotMapperFactory() {
        return Either.right(valueAlgebra.slotMapperFactory());
    }

    @Override
    public Either<ReactiveValue, Value> kernelString(Set<SourceFileRange> ranges, String text, Either<ReactiveValue, Value> trueValue) {
        return Either.right(valueAlgebra.kernelString(ranges, text, trueValue.right().value()));
    }

    @Override
    public Either<ReactiveValue, Value> kernelNumber(Set<SourceFileRange> ranges, Number number, Either<ReactiveValue, Value> trueValue) {
        return Either.right(valueAlgebra.kernelNumber(ranges, number, trueValue.right().value()));
    }

    @Override
    public Either<ReactiveValue, Value> kernelBoolean(Set<SourceFileRange> ranges, boolean value, Either<ReactiveValue, Value> trueValue) {
        return Either.right(valueAlgebra.kernelBoolean(ranges, value, trueValue.right().value()));
    }

    @Override
    public Either<ReactiveValue, Value> failFunction() {
        return Either.right(valueAlgebra.failFunction());
    }

    @Override
    public Either<ReactiveValue, Value> extendFunction() {
        return Either.right(valueAlgebra.extendFunction());
    }

    @Override
    public Either<ReactiveValue, Value> unboundIdentifier(Set<SourceFileRange> ranges, String name) {
        return Either.right(valueAlgebra.unboundIdentifier(ranges, name));
    }

    @Override
    public Either<ReactiveValue, Value> badExpr(Set<SourceFileRange> ranges, String message) {
        return Either.right(valueAlgebra.badExpr(ranges, message));
    }

}
