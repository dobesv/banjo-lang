package banjo.dom.source;

import static banjo.parser.util.Check.nonNull;

import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

/**
 * Empty expressions help with the special case of empty parentheses: (), [], and {}.  These
 * are parsed as a parenthese with the "empty" expression inside.  This may also show up in
 * a list with consecutive or trailing separators.
 */
public class EmptyExpr extends AbstractCompositeSourceExpr implements SourceExpr {

	public EmptyExpr(List<SourceNode> children) {
		super(children);
	}

	public EmptyExpr() {
		this(nonNull(Collections.<SourceNode>emptyList()));
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.emptyExpr(this);
	}

}