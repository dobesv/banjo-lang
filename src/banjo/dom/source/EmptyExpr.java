package banjo.dom.source;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

/**
 * Empty expressions help with the special case of empty parentheses: (), [], and {}.  These
 * are parsed as a parenthese with the "empty" expression inside.  This may also show up in
 * a list with consecutive or trailing separators.
 */
public class EmptyExpr extends AbstractOp implements SourceExpr {
	public static final SourceExpr SYNTHETIC_INSTANCE = new EmptyExpr(List.<SourceFileRange>nil());

	public EmptyExpr(List<SourceFileRange> ranges) {
		super(ranges, Operator.EMPTY);
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.emptyExpr(this);
	}

	@Override
	public void toSource(StringBuffer sb) {
		// Empty ... do nothing
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.nil();
	}

}
