package banjo.expr.free;

import banjo.eval.EvalContext;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.NameRefAlgebra;
import banjo.eval.resolver.Resolver;
import banjo.expr.util.SourceFileRange;
import fj.data.Option;
import fj.data.Set;


public class FreeProjection implements FreeExpression {
	public final FreeExpression object;
	public final FreeExpression projection;


	public FreeProjection(FreeExpression object, FreeExpression projection) {
        super();
        this.object = object;
        this.projection = projection;
    }

	@Override
    public Set<NameRef> getFreeRefs() {
        // We propagate the free refs of the object we are looking at, and any
        // global refs inside the projection. Other references inside the
        // projection can not refer to anything outside the object and so are
        // not "free" refs for this purpose
        Set<NameRef> objectFreeRefs = object.getFreeRefs();
        Set<NameRef> projectionFreeRefs = projection.getFreeRefs();
        return objectFreeRefs.union(projectionFreeRefs.filter(FreeProjection::isGlobal));
    }

    private static Boolean isGlobal(NameRef ref) {
        return ref.acceptVisitor(new NameRefAlgebra<Boolean>() {

            @Override
            public Boolean local(Set<SourceFileRange> ranges, String name) {
                return false;
            }

            @Override
            public Boolean slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
                // Propagate if its a slot in a global ref
                return object.acceptVisitor(this);
            }

            @Override
            public Boolean baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
                return false;
            }

            @Override
            public Boolean functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
                return false;
            }

            @Override
            public Boolean invalid(Set<SourceFileRange> ranges, String reason) {
                return true;
            }

        });
    }

    @Override
    public boolean hasFreeRefs() {
        return object.hasFreeRefs() || !getFreeRefs().isEmpty();
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver resolver) {
        Option<FreeExpression> newObject = this.object.partial(resolver);
        // In the "body" of the projection, no free variables escape, only
        // global references
        Option<FreeExpression> newProjection = this.projection.partial(new BodyPartialResolver(resolver, name -> true));
        if(newObject.isNone() && newProjection.isNone())
            return Option.none();

        return Option.some(FreeExpression.projection(newObject.orSome(object), newProjection.orSome(projection)));
    }

    @Override
    public <T> T eval(EvalContext<T> ctx, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        final T object = this.object.eval(ctx, resolver, algebra);
        return apply(ctx, resolver, algebra, object, projection);
    }

    public static <T> T apply(EvalContext<T> ctx, Resolver<T> resolver, InstanceAlgebra<T> algebra, final T object, final FreeExpression projection) {
        return projection.eval(ctx, projectionResolver(resolver, algebra, object), algebra);
    }

    public static <T> Resolver<T> projectionResolver(Resolver<T> resolver, InstanceAlgebra<T> algebra, final T object) {
        return new ProjectionResolver<T>(resolver, algebra, object);
    }

	@Override
	public String toString() {
	    return object+"."+projection;
	}

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.projection(this);
    }
}