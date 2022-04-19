package banjo.eval.resolver;

import banjo.eval.EvalContext;
import banjo.expr.core.CoreExpr;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionFactory;
import banjo.expr.free.FreeProjection;
import banjo.expr.util.SourceFileRange;
import fj.P;
import fj.data.List;
import fj.data.Set;
import fj.data.TreeMap;

public interface Resolver<T> extends NameRefAlgebra<T> {
    public static class CompositeResolver<T> extends WrappedResolver<T> {
        final NameRefAlgebra<NameRef> translation;

        public CompositeResolver(Resolver<T> delegate, NameRefAlgebra<NameRef> translation) {
            super(delegate);
            this.translation = translation;
        }

        @Override
        public T slot(NameRef object, Set<SourceFileRange> ranges, String slotName) {
            return translation.slot(object, ranges, slotName).acceptVisitor(delegate);
        }

        @Override
        public T local(Set<SourceFileRange> ranges, String name) {
            return translation.local(ranges, name).acceptVisitor(delegate);
        }

        @Override
        public T invalid(Set<SourceFileRange> ranges, String reason) {
            return translation.invalid(ranges, reason).acceptVisitor(delegate);
        }

        @Override
        public T functionBase(Set<SourceFileRange> ranges, String functionSelfName) {
            return translation.functionBase(ranges, functionSelfName).acceptVisitor(delegate);
        }

        @Override
        public T baseSlot(Set<SourceFileRange> ranges, String slotObjectRef, String slotName) {
            return translation.baseSlot(ranges, slotObjectRef, slotName).acceptVisitor(delegate);
        }
    }

    public default T apply(NameRef ref) {
        return visit(ref);
    }

    public default Resolver<T> compose(NameRefAlgebra<NameRef> translation) {
        return new CompositeResolver<T>(this, translation);
    }

    public static <T> Resolver<T> empty(InstanceAlgebra<T> algebra) {
        return new EmptyResolver<T>(algebra);
    }
    public default Resolver<T> projection(T object, InstanceAlgebra<T> algebra) {
        return FreeProjection.projectionResolver(this, algebra, object);
    }

    public default T eval(CoreExpr expr, T projectRoot, InstanceAlgebra<T> algebra) {
        EvalContext<T> ctx = new EvalContext<T>(List.nil(), projectRoot);
        FreeExpression fx = FreeExpressionFactory.apply(expr);
        return fx.eval(ctx, this, algebra);
    }

    public default TreeMap<NameRef, T> closure(Set<NameRef> refs) {
        return TreeMap.iterableTreeMap(NameRef.ORD, refs.toList().map(ref -> P.p(ref, apply(ref))));
    }


}
