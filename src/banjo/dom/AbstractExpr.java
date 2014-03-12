package banjo.dom;


import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.parser.util.AbstractCachedHashCode;
import banjo.parser.util.SourceFileRange;

public abstract class AbstractExpr extends AbstractCachedHashCode implements Expr {
	final SourceFileRange sourceFileRange;

	public AbstractExpr(int hashCode, SourceFileRange sourceFileRange) {
		super(hashCode);
		this.sourceFileRange = sourceFileRange;
	}

	@Override
	public SourceFileRange getSourceFileRange() {
		return this.sourceFileRange;
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
		@SuppressWarnings("null") @NonNull
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
		@SuppressWarnings("null") @NonNull
		final String result = buf.toString();
		return result;
	}


	@Override
	public String toString() {
		return toSource();
	}

	static @Nullable
	protected <T extends Expr> T optTransform(@Nullable T opt, ExprTransformer transformer) {
		if(opt == null)
			return opt;
		final T value = nonNull(opt);
		final T newValue = transformer.transform(value);
		if(value == newValue)
			return opt;
		return newValue;
	}

	@Override
	public int compareTo(Expr o) {
		return this.sourceFileRange.compareTo(o.getSourceFileRange());
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		return obj == this || (
				super.equals(obj) &&
				(obj instanceof Expr) &&
				this.sourceFileRange.equals(((Expr)obj).getSourceFileRange())
				);
	}
}
