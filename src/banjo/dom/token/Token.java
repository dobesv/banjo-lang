package banjo.dom.token;


/**
 * A token is a parse tree node which has no sub-expression.  A token may also
 * be whitespace, comments, and operators that are not normally included in the
 * parse tree result.
 */
public interface Token {
	public void toSource(StringBuffer sb);

	public String toSource();

}
