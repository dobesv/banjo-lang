package banjo.dom.token;

import static banjo.parser.util.Check.nonNull;
import banjo.dom.Expr;
import banjo.dom.source.AbstractSourceNode;
import banjo.dom.source.SourceExpr;



public abstract class AbstractAtom extends AbstractSourceNode implements Atom {

	public AbstractAtom(int sourceLength, int hashCode) {
		super(sourceLength, hashCode);
	}

	@Override
	public SourceExpr getSourceExpr() {
		return this;
	}

	@Override
	public Class<? extends Expr> getExprClass() {
		return nonNull(getClass());
	}

}
