package banjo.dom;

import banjo.dom.source.Precedence;
import banjo.parser.util.SourceFileRange;



public interface Expr extends Comparable<Expr> {

	public void toSource(StringBuffer sb);

	public void toSource(StringBuffer sb, Precedence outerPrec);

	public String toSource(Precedence prec);

	public String toSource();

	public Precedence getPrecedence();

	public SourceFileRange getSourceFileRange();
}