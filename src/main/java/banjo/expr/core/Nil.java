package banjo.expr.core;

import banjo.expr.source.EmptyExpr;
import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.UnaryOp;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

public class Nil extends AbstractCoreExpr implements CoreExpr {
	public static final Nil SYNTHETIC_INSTANCE = new Nil(SourceFileRange.EMPTY_SET);

	public Nil(Set<SourceFileRange> sourceFileRanges) {
		super(0, sourceFileRanges);
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.lowest();
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.nil();
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.nil();
	}

	@Override
	public SourceExpr toSourceExpr() {
		return new UnaryOp(Operator.OBJECT_COMPOSITION, this.getRanges(), EmptyExpr.SYNTHETIC_INSTANCE);
	}

	public static boolean isNil(CoreExpr e) {
		return e == SYNTHETIC_INSTANCE || e.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
			@Override
			public Boolean fallback() {
				return false;
			}

			@Override
			public Boolean nil() {
				return true;
			}
		});
	}
}
