package banjo.expr.free;

import banjo.eval.EvalContext;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.NameRefAlgebra;
import banjo.eval.resolver.Resolver;
import banjo.expr.source.Operator;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.data.Option;
import fj.data.Set;

/**
 * Projection that will use the "base" version of the current slot.
 * 
 * This is a bit tricky to understand and implement, unfortunately.  The
 * "base" slot value will be used for
 */
public class FreeBaseProjection implements FreeExpression {
    public final Set<SourceFileRange> ranges;
    public final String slotObjectName;
	public final FreeExpression projection;


    public FreeBaseProjection(Set<SourceFileRange> ranges, String slotObjectName, FreeExpression projection) {
        super();
        this.ranges = ranges;
        this.slotObjectName = slotObjectName;
        this.projection = projection;
    }

    public FreeBaseProjection(Identifier object, FreeExpression projection) {
        super();
        this.ranges = object.ranges;
        this.slotObjectName = object.id;
        this.projection = projection;
    }

    @Override
	public Set<NameRef> getFreeRefs() {
	    // Now we have to translate the free refs inside the projection
        // into refs from outside the projection.
        return projection.getFreeRefs().map(NameRef.ORD, this::translateNameRef);
    }

    @Override
    public boolean hasFreeRefs() {
        return projection.hasFreeRefs();
    }

    public NameRef translateNameRef(NameRef ref) {
        return ref.acceptVisitor(this.nameTranslation());
    }

    public NameRefAlgebra<NameRef> nameTranslation() {
        return new NameRefAlgebra<NameRef>() {

            @Override
            public NameRef local(Set<SourceFileRange> ranges, String name) {
                // For a plain identifier, tack on our prefix
                return NameRef.baseSlot(SourceFileRange.union(ranges, ranges), slotObjectName, name);
            }

            @Override
            public NameRef slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
                // For a slot reference, we must adjust the target object of the
                // slot reference
                return NameRef.slot(object.acceptVisitor(this), ranges, slotName);
            }

            @Override
            public NameRef baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
                // Can't use a base slot in this context
                return NameRef.invalid(ranges, slotObjectRef + " is not a slot object name; it's a regular slot name");
            }

            @Override
            public NameRef functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
                // Can't reference a base function in this context
                return NameRef.invalid(ranges, functionSelfName + " is not a function self name; it's a regular slot name");
            }

            @Override
            public NameRef invalid(Set<SourceFileRange> ranges, String reason) {
                // Propagate errors we already found
                return NameRef.invalid(ranges, reason);
            }

        };
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver resolver) {
        Option<FreeExpression> newProjection = projection.partial(resolver.compose(this.nameTranslation()));
        // If the projection has no free variables, it's not useful to wrap it
        // in our scope any more; when exactly this would happen, I'm not sure.
        return newProjection.map(p -> p.hasFreeRefs() ? new FreeBaseProjection(ranges, slotObjectName, projection) : p);
    }

    @Override
    public <T> T eval(EvalContext<T> ctx, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        return projection.eval(ctx, resolver.compose(this.nameTranslation()), algebra);
    }

	@Override
	public String toString() {
        return this.slotObjectName + Operator.BASE_SLOT.getOp() + projection;
	}

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.baseProjection(this);
    }
}