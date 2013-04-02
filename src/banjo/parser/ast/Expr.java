package banjo.parser.ast;

import banjo.parser.util.FileRange;

public interface Expr {

	public int getStartColumn();
	public FileRange getFileRange();

	public void toSource(StringBuffer sb);

	public Precedence getPrecedence();

	public void toSource(StringBuffer sb, Precedence outerPrec);

	public String toSource(Precedence prec);

	public String toSource();

}