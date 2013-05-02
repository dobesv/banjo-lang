package banjo.dom;

import banjo.dom.source.Precedence;



public interface Expr {

	public void toSource(StringBuffer sb);

	public void toSource(StringBuffer sb, Precedence outerPrec);

	public String toSource(Precedence prec);

	public String toSource();

	public Precedence getPrecedence();

}