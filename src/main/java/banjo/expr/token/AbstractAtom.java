package banjo.expr.token;

import banjo.expr.AbstractExpr;
import banjo.expr.source.Precedence;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;




public abstract class AbstractAtom extends AbstractExpr {
	public final int indentColumn;

	public AbstractAtom(Set<SourceFileRange> ranges, int indentColumn) {
		super(ranges);
		this.indentColumn = indentColumn;
	}
	public AbstractAtom(SourceFileRange range, int indentColumn) {
		this(Set.single(SourceFileRange.ORD, range), indentColumn);
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}


	@Override
	public void toSource(StringBuffer sb, Precedence outerPrec) {
		toSource(sb);
	}

    @Override
    public String toSource() {
        StringBuffer sb = new StringBuffer();
        toSource(sb);
        return sb.toString();
	}
}
