package banjo.dom.token;

import banjo.dom.source.AbstractSourceNode;
import banjo.dom.source.Atom;
import banjo.dom.source.SourceExpr;



public abstract class AbstractAtom extends AbstractSourceNode implements Atom {

	public AbstractAtom(int sourceLength) {
		super(sourceLength);
	}

	@Override
	public SourceExpr getSourceExpr() {
		return this;
	}


}
