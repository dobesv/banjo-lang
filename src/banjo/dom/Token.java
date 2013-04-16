package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

/**
 * A token is a parse tree node which has no sub-expression.  A token may also
 * be whitespace, comments, and operators that are not normally included in the
 * parse tree result.
 */
public interface Token {
	@Nullable
	<T> T acceptVisitor(TokenVisitor<T> visitor);

}
