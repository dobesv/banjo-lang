package banjo.eval.resolver;

import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionVisitor;
import banjo.expr.free.PartialResolver;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public enum GlobalRef implements NameRef,FreeExpression {
    EMPTY_LIST(Identifier.EMPTY_LIST.id),
    SINGLE_ELEMENT_LIST_FACTORY(Identifier.SINGLE_ELEMENT_LIST.id),
    FUNCTION_TRAIT(Identifier.FUNCTION_TRAIT.id),
    LANGUAGE_KERNEL_NUMBER(Identifier.LANGUAGE_KERNEL_NUMBER.id),
    LANGUAGE_KERNEL_STRING(Identifier.LANGUAGE_KERNEL_STRING.id),
    TRUE(Identifier.TRUE.id);

    public final String name;

    private GlobalRef(String name) {
        this.name = name;
    }

    @Override
    public Set<SourceFileRange> getRanges() {
        return SourceFileRange.EMPTY_SET;
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public <T> T acceptVisitor(NameRefAlgebra<T> visitor) {
        return visitor.global(this);
    }

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.global(this);
    }

    @Override
    public Set<NameRef> getFreeRefs() {
        return Set.single(NameRef.ORD, this);
    }

    @Override
    public boolean hasFreeRefs() {
        return true;
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver resolver) {
        return Option.none();
    }

    @Override
    public <T> T eval(List<T> trace, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        return resolver.global(this);
    }

}
