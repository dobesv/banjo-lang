package banjo.dom;


import static banjo.parser.util.Check.nonNull;
import banjo.dom.source.Precedence;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public abstract class AbstractExpr implements Expr {
	final List<SourceFileRange> sourceFileRanges;

	public AbstractExpr(List<SourceFileRange> sourceFileRanges) {
		this.sourceFileRanges = sourceFileRanges;
	}

	@Override
	public List<SourceFileRange> getSourceFileRanges() {
		return this.sourceFileRanges;
	}


	@Override
	public void toSource(StringBuffer sb, Precedence outerPrec) {
		final Precedence prec = getPrecedence();
		final boolean needParens = prec != Precedence.ATOM && outerPrec != prec && outerPrec.isHigherThan(prec);
		if(needParens) sb.append('(');
		toSource(sb);
		if(needParens) sb.append(')');
	}

	@Override
	public String toSource(Precedence prec) {
		final StringBuffer buf = new StringBuffer();
		toSource(buf, prec);

		final String result = buf.toString();
		return result;
	}

	@Override
	public String toSource() {
		return toSource(Precedence.lowest());
	}

	public void toFullyParenthesizedSource(StringBuffer sb) {
		toSource(sb);
	}

	public String toFullyParenthesizedSource() {
		final StringBuffer buf = new StringBuffer();
		toFullyParenthesizedSource(buf);

		final String result = buf.toString();
		return result;
	}


	@Override
	public String toString() {
		return toSource();
	}

	static
	protected <T extends Expr> T optTransform(T opt, ExprTransformer transformer) {
		if(opt == null)
			return opt;
		final T value = nonNull(opt);
		final T newValue = transformer.transform(value);
		if(value == newValue)
			return opt;
		return newValue;
	}

}
