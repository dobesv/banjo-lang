package banjo.dom.token;

import banjo.dom.AbstractExpr;
import banjo.dom.source.Precedence;
import banjo.parser.util.SourceFileRange;




public abstract class AbstractAtom extends AbstractExpr {

	public AbstractAtom(int hashCode, SourceFileRange sfr) {
		super(hashCode, sfr);
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
