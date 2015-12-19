package banjo.expr.core;

import banjo.expr.AbstractBadExpr;
import banjo.expr.BadExpr;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class BadCoreExpr extends AbstractBadExpr implements CoreExpr, BadExpr {
	public static final class UnknownType extends BadCoreExpr {
		public UnknownType(Set<SourceFileRange> ranges) {
			super(ranges, "Unable to determine type");
		}
	}

	public BadCoreExpr(Set<SourceFileRange> ranges, String messageTemplate, Object ... args) {
		super(ranges, messageTemplate, args);
	}

	public BadCoreExpr(SourceFileRange sourceFileRange, String messageTemplate, Object ... args) {
		this(Set.single(SourceFileRange.ORD, sourceFileRange), messageTemplate, args);
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.badExpr(this);
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return super.acceptVisitor(visitor);
	}
}
