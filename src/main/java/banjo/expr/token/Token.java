package banjo.expr.token;



/**
 * A token is a parse tree node which has no sub-expression.  A token may also
 * be whitespace, comments, and operators that are not normally included in the
 * parse tree result.
 */
public interface Token {
	public <T> T acceptVisitor(TokenVisitor<T> parser);

}
