package banjo.dom.source;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.SourceFileRange;

/**
 * Empty expressions help with the special case of empty parentheses: (), [], and {}.  These
 * are parsed as a parenthese with the "empty" expression inside.  This may also show up in
 * a list with consecutive or trailing separators.
 */
public class EmptyExpr extends AbstractOp implements SourceExpr {
	public static final SourceExpr SYNTHETIC_INSTANCE = new EmptyExpr(SourceFileRange.SYNTHETIC);

	public EmptyExpr(SourceFileRange sfr) {
		super(sfr, Operator.EMPTY);
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

}
