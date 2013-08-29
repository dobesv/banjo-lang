package banjo.dom.token;

import banjo.dom.AbstractExpr;
import banjo.dom.source.Precedence;




public abstract class AbstractAtom extends AbstractExpr {

	public AbstractAtom(int hashCode) {
		super(hashCode);
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
