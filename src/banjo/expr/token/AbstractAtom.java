package banjo.expr.token;

import banjo.expr.AbstractExpr;
import banjo.expr.source.Precedence;
import banjo.expr.util.SourceFileRange;
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
