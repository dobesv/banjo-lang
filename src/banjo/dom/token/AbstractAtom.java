package banjo.dom.token;

import fj.data.List;
import banjo.dom.AbstractExpr;
import banjo.dom.source.Precedence;
import banjo.parser.util.SourceFileRange;




public abstract class AbstractAtom extends AbstractExpr {

	public AbstractAtom(int hashCode, List<SourceFileRange> ranges) {
		super(hashCode, ranges);
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}


	@Override
	public void toSource(StringBuffer sb, Precedence outerPrec) {
		toSource(sb);
	}

}
