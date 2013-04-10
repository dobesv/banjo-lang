package banjo.parser.ast;


public interface Expr extends HasFileRange {

	public void toSource(StringBuffer sb);

	public Precedence getPrecedence();

	public void toSource(StringBuffer sb, Precedence outerPrec);

	public String toSource(Precedence prec);

	public String toSource();

}