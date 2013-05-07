package banjo.dom;


import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.parser.util.AbstractCachedHashCode;

public abstract class AbstractExpr extends AbstractCachedHashCode implements Expr {

	public AbstractExpr(int hashCode) {
		super(hashCode);
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
		return nonNull(buf.toString());
	}

	/**
	 * Get the offset of this expression in its "parent" expression (in characters).  In
	 * general this is provided by a wrapper around an expression that adds an offset.  This
	 * allows expression subtrees to be shared around more easily because the wrapper can be
	 * discarded without affecting the inner object.
	 */
	@Override
	public int getOffsetInParent() {
		return 0;
	}

	@Override
	public String toSource() {
		return toSource(Precedence.lowest());
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
	public Class<? extends Expr> getExprClass() {
		return nonNull(getClass());
	}

}
