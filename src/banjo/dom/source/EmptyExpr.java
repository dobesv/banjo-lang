package banjo.dom.source;

import banjo.dom.AbstractExpr;
import banjo.dom.BadExpr;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

/**
 * Empty expressions help with the special case of empty parentheses: (), [], and {}.  These
 * are parsed as a parenthese with the "empty" expression inside.  This may also show up in
 * a list with consecutive or trailing separators.
 */
public class EmptyExpr extends AbstractExpr implements SourceExpr {
	public static final SourceExpr SYNTHETIC_INSTANCE = new EmptyExpr(SourceFileRange.EMPTY_LIST);

	public EmptyExpr(List<SourceFileRange> ranges) {
		super(733512, ranges);
	}
	public EmptyExpr(SourceFileRange range) {
		this(List.single(range));
	}

	public EmptyExpr() {
		this(List.nil());
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
