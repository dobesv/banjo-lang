package banjo.dom;

import fj.data.List;
import banjo.dom.core.ExprVisitor;
import banjo.dom.source.Precedence;
import banjo.parser.util.SourceFileRange;



public interface Expr extends Comparable<Expr> {

	public void toSource(StringBuffer sb);

	public void toSource(StringBuffer sb, Precedence outerPrec);

	public String toSource(Precedence prec);

	public String toSource();

	public Precedence getPrecedence();

	public List<SourceFileRange> getSourceFileRanges();

}