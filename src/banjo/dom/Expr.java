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
	 * Return the class of this expression
	 */
	Class<? extends Expr> getExprClass();


}