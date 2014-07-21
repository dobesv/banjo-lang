package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import fj.data.List;
import banjo.dom.AbstractBadExpr;
import banjo.dom.BadExpr;
import banjo.dom.ExprAlgebra;
import banjo.parser.util.SourceFileRange;

public class BadCoreExpr extends AbstractBadExpr implements CoreExpr, BadExpr {
	public static final class UnknownType extends BadCoreExpr {
		public UnknownType(List<SourceFileRange> ranges) {
			super(ranges, "Unable to determine type");
		}
	}

	public BadCoreExpr(List<SourceFileRange> ranges, String messageTemplate, Object ... args) {
		super(ranges, messageTemplate, args);
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return super.acceptVisitor(visitor);
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return super.acceptVisitor(visitor);
	}
}
