package banjo.dom.core;

import banjo.dom.AbstractBadExpr;
import banjo.dom.BadExpr;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class BadCoreExpr extends AbstractBadExpr implements CoreExpr, BadExpr {
	public static final class UnknownType extends BadCoreExpr {
		public UnknownType(List<SourceFileRange> ranges) {
			super(ranges, "Unable to determine type");
		}
	}

	public BadCoreExpr(List<SourceFileRange> ranges, String messageTemplate, Object ... args) {
		super(ranges, messageTemplate, args);
	}

	public BadCoreExpr(SourceFileRange sourceFileRange, String messageTemplate, Object ... args) {
		this(List.single(sourceFileRange), messageTemplate, args);
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
