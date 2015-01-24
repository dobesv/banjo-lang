package banjo.dom;

import banjo.dom.source.Precedence;
import banjo.parser.util.SourceFileRange;
import fj.data.List;



public interface Expr extends Comparable<Expr> {

	public void toSource(StringBuffer sb, String idPrefix);

	public void toSource(StringBuffer sb, Precedence outerPrec, String idPrefix);

	public String toSource(Precedence prec, String idPrefix);

	public String toSource();

	public Precedence getPrecedence();

	public List<SourceFileRange> getSourceFileRanges();

}