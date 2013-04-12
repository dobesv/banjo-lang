package banjo.dom;


import banjo.parser.util.FileRange;

public abstract class AbstractExpr extends AbstractHasFileRange implements Expr {
	public AbstractExpr(FileRange range) {
		super(range);
	}
	
	@Override
	public abstract void toSource(StringBuffer sb);
	@Override
	public abstract Precedence getPrecedence();
	@Override
	public void toSource(StringBuffer sb, Precedence outerPrec) {
		final Precedence prec = getPrecedence();
		boolean needParens = prec != Precedence.ATOM && outerPrec != prec && outerPrec.isHigherThan(prec);
		if(needParens) sb.append('(');
		toSource(sb);
		if(needParens) sb.append(')');
	}
	
	@Override
	public String toSource(Precedence prec) {
		StringBuffer buf = new StringBuffer();
		toSource(buf, prec);
		return buf.toString();
	}
	
	@Override
	public String toSource() {
		return toSource(Precedence.lowest());
	}
	
	public String toString() {
		return toSource();
	}
}
