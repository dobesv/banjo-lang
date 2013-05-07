package banjo.dom;

import banjo.dom.source.Precedence;



public interface Expr {

	public void toSource(StringBuffer sb);

	public void toSource(StringBuffer sb, Precedence outerPrec);

	public String toSource(Precedence prec);

	public String toSource();

	public Precedence getPrecedence();

	/**
	 * @return The offset of this node in it's "parent" node.
	 */
	public int getOffsetInParent();

	/**
	 * @return The number of characters of the source code covered by this node.  If this node is synthetic, this may be zero.
	 */
	public int getSourceLength();

	/**
	 * Return the class of this expression
	 */
	Class<? extends Expr> getExprClass();


}