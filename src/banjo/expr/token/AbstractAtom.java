package banjo.expr.token;

import banjo.expr.AbstractExpr;
import banjo.expr.source.Precedence;
import banjo.expr.util.SourceFileRange;
import fj.data.List;




public abstract class AbstractAtom extends AbstractExpr {
	public final int indentColumn;

	public AbstractAtom(List<SourceFileRange> ranges, int indentColumn) {
		super(ranges);
		this.indentColumn = indentColumn;
	}
	public AbstractAtom(SourceFileRange range, int indentColumn) {
		this(List.single(range), indentColumn);
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
