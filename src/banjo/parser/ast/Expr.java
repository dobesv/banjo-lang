package banjo.parser.ast;


import banjo.parser.util.FileRange;

public abstract class Expr {
	private final FileRange fileRange;

	public Expr(FileRange range) {
		super();
		this.fileRange = range;
	}
	
	public int getStartColumn() { return getFileRange().getStart().getColumn(); }

	public FileRange getFileRange() {
		return fileRange;
	}
	
	public abstract void toSource(StringBuffer sb);
	public abstract Precedence getPrecedence();
	public void toSource(StringBuffer sb, Precedence outerPrec) {
		final Precedence prec = getPrecedence();
		boolean needParens = prec != Precedence.ATOM && outerPrec != prec && outerPrec.isHigherThan(prec);
		if(needParens) sb.append('(');
		toSource(sb);
		if(needParens) sb.append(')');
	}
	
	public String toSource(Precedence prec) {
		StringBuffer buf = new StringBuffer();
		toSource(buf, prec);
		return buf.toString();
	}
	
	public String toSource() {
		return toSource(Precedence.lowest());
	}
	
	public String toString() {
		return toSource();
	}
}
