package banjo.dom.token;

import banjo.dom.AbstractExpr;
import banjo.dom.source.Precedence;
import banjo.parser.util.SourceFileRange;
import fj.data.List;




public abstract class AbstractAtom extends AbstractExpr {

	public AbstractAtom(List<SourceFileRange> ranges) {
		super(ranges);
	}
	public AbstractAtom(SourceFileRange range) {
		this(List.single(range));
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}


	@Override
	public void toSource(StringBuffer sb, Precedence outerPrec) {
		toSource(sb);
	}

	public void toSource(StringBuffer sb) {
		toSource(sb);
	}
}
