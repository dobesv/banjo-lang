package banjo.expr.free;

import java.util.function.Predicate;

import banjo.eval.resolver.GlobalRef;
import banjo.eval.resolver.NameRef;
import banjo.expr.util.SourceFileRange;
import fj.data.Option;
import fj.data.Set;

public class BodyPartialResolver implements PartialResolver {
    final PartialResolver delegate;
    final Predicate<String> isParameter;

    public BodyPartialResolver(PartialResolver resolver, Predicate<String> isParameter) {
        this.delegate = resolver;
        this.isParameter = isParameter;
    }

    boolean isParameter(String name) {
        return isParameter.test(name);
    }

    @Override
    public Option<FreeExpression> local(Set<SourceFileRange> ranges, String name) {
        // Pass-through if the name we're pulling the slot from is not a
        // parameter or function self-name
        if(isParameter(name)) {
            return Option.none();
        }
        return delegate.local(ranges, name);
    }

    @Override
    public Option<FreeExpression> slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
        // Pass-through if the name we're pulling the slot from is not a
        // parameter or function self-name
        return object.acceptVisitor(this).bind((v) -> delegate.slot(object, ranges, slotName));
    }

    @Override
    public Option<FreeExpression> baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
        // Pass-through if the name we're pulling the slot from is not a
        // parameter or function self-name
        if(isParameter(slotObjectRef)) {
            return Option.none();
        }
        return delegate.baseSlot(ranges, slotObjectRef, slotName);
    }

    @Override
    public Option<FreeExpression> functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
        if(isParameter(functionSelfName))
            return Option.none();
        return delegate.functionBase(ranges, functionSelfName);
    }

    @Override
    public Option<FreeExpression> invalid(Set<SourceFileRange> ranges, String reason) {
        return delegate.invalid(ranges, reason);
    }

    @Override
    public Option<FreeExpression> global(GlobalRef globalRef) {
        return delegate.global(globalRef);
    }
}