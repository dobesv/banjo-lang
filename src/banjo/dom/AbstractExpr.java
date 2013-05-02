package banjo.dom;


import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;

public abstract class AbstractExpr implements Expr {
	
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
		return nonNull(buf.toString());
	}
	
	@Override
	public String toSource() {
		return toSource(Precedence.lowest());
	}
	
	public String toString() {
		return toSource();
	}
	
	static @Nullable
	protected <T extends Expr> T optTransform(@Nullable T opt, ExprTransformer transformer) {
		if(opt == null)
			return opt;
		T value = nonNull(opt);
		T newValue = transformer.transform(value);
		if(value == newValue)
			return opt;
		return newValue;
	}
}
