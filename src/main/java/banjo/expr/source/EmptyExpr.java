package banjo.expr.source;

import banjo.expr.AbstractExpr;
import banjo.expr.BadExpr;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

/**
 * Empty expressions help with the special case of empty parentheses: (), [], and {}.  These
 * are parsed as a parenthese with the "empty" expression inside.  This may also show up in
 * a list with consecutive or trailing separators.
 */
public class EmptyExpr extends AbstractExpr implements SourceExpr {
	public static final SourceExpr SYNTHETIC_INSTANCE = new EmptyExpr(SourceFileRange.EMPTY_SET);

	public EmptyExpr(Set<SourceFileRange> ranges) {
		super(ranges);
	}
	public EmptyExpr(SourceFileRange range) {
		this(Set.single(SourceFileRange.ORD, range));
	}

	public EmptyExpr() {
		this(SourceFileRange.EMPTY_SET);
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.emptyExpr(this);
	}
	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.emptyExpr(getSourceFileRanges());
	}

	@Override
	public void toSource(StringBuffer sb) {
		// Empty ... do nothing
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.nil();
	}
	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

}
