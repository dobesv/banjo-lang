package banjo.expr;


import java.util.Objects;

import banjo.expr.source.Precedence;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public abstract class AbstractExpr implements Expr {
	final Set<SourceFileRange> sourceFileRanges;

	public AbstractExpr(Set<SourceFileRange> sourceFileRanges) {
		if(sourceFileRanges == null) throw new NullPointerException();
		this.sourceFileRanges = sourceFileRanges;
	}

	@Override
	public Set<SourceFileRange> getSourceFileRanges() {
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
		final T value1 = opt;
		final T value = Objects.requireNonNull(value1);
		final T newValue = transformer.transform(value);
		if(value == newValue)
			return opt;
		return newValue;
	}

}
