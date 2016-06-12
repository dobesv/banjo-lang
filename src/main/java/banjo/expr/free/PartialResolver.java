package banjo.expr.free;

import banjo.eval.resolver.GlobalRef;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.NameRefAlgebra;
import banjo.expr.util.SourceFileRange;
import fj.data.Option;
import fj.data.Set;

public interface PartialResolver extends NameRefAlgebra<Option<FreeExpression>> {
    public default Option<FreeExpression> apply(NameRef ref) {
        return ref.acceptVisitor(this);
    }

    public default PartialResolver compose(NameRefAlgebra<NameRef> translation) {
        PartialResolver visitor = this;
        return new PartialResolver() {

            @Override
            public Option<FreeExpression> slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
                return translation.slot(object, ranges, slotName).acceptVisitor(visitor);
            }

            @Override
            public Option<FreeExpression> local(Set<SourceFileRange> ranges, String name) {
                return translation.local(ranges, name).acceptVisitor(visitor);
            }

            @Override
            public Option<FreeExpression> invalid(Set<SourceFileRange> ranges, String reason) {
                return translation.invalid(ranges, reason).acceptVisitor(visitor);
            }

            @Override
            public Option<FreeExpression> functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
                return translation.functionBase(ranges, functionSelfName).acceptVisitor(visitor);
            }

            @Override
            public Option<FreeExpression> baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
                return translation.baseSlot(ranges, slotObjectRef, slotName).acceptVisitor(visitor);
            }

            @Override
            public Option<FreeExpression> global(GlobalRef globalRef) {
                return translation.global(globalRef).acceptVisitor(visitor);
            }
        };
    }


    @Override
    public default Option<FreeExpression> baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
        return Option.none();
    }

    @Override
    public default Option<FreeExpression> functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
        return Option.none();
    }

    @Override
    public default Option<FreeExpression> invalid(Set<SourceFileRange> ranges, String reason) {
        return Option.none();
    }

    @Override
    public default Option<FreeExpression> slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
        return Option.none();
    }

    @Override
    public default Option<FreeExpression> local(Set<SourceFileRange> ranges, String name) {
        return Option.none();
    }

    @Override
    default Option<FreeExpression> global(GlobalRef globalRef) {
        return Option.none();
    }

    public default PartialResolver andThen(PartialResolver second) {
        PartialResolver first = this;
        return new PartialResolver() {
            @Override
            public Option<FreeExpression> baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
                return first.baseSlot(ranges, slotObjectRef, slotName).orElse(() -> second.baseSlot(ranges, slotObjectRef, slotName));
            }

            @Override
            public Option<FreeExpression> functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
                return first.functionBase(ranges, functionSelfName).orElse(() -> second.functionBase(ranges, functionSelfName));
            }

            @Override
            public Option<FreeExpression> invalid(Set<SourceFileRange> ranges, String reason) {
                return first.invalid(ranges, reason).orElse(() -> second.invalid(ranges, reason));
            }

            @Override
            public Option<FreeExpression> local(Set<SourceFileRange> ranges, String name) {
                return first.local(ranges, name).orElse(() -> second.local(ranges, name));
            }

            @Override
            public Option<FreeExpression> slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
                return first.slot(object, ranges, slotName).orElse(() -> second.slot(object, ranges, slotName));
            }

            @Override
            public Option<FreeExpression> global(GlobalRef globalRef) {
                return first.global(globalRef).orElse(() -> second.global(globalRef));
            }
            @Override
            public Option<FreeExpression> apply(NameRef ref) {
                return first.apply(ref).orElse(() -> second.apply(ref));
            }
        };
    }

}
