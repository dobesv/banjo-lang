package banjo.parser.util;

import banjo.dom.Expr;
import banjo.dom.source.Precedence;

/**
 * Wrap an expression with an offset.
 */
public class OffsetExpr<T extends Expr> extends OffsetValue<T> implements Expr {
	public OffsetExpr(int offset, T value) {
		super(offset, value);
	}
	public OffsetExpr(OffsetValue<?> offset, Problematic<T> value) {
		super(offset, value.getValue());
	}
	public OffsetExpr(OffsetValue<?> offset, T value) {
		super(offset, value);
	}
	@Override
	public void toSource(StringBuffer sb) {
		this.value.toSource(sb);
	}
	@Override
	public void toSource(StringBuffer sb, Precedence outerPrec) {
		this.value.toSource(sb, outerPrec);
	}
	@Override
	public String toSource(Precedence prec) {
		return this.value.toSource(prec);
	}
	@Override
	public String toSource() {
		return this.value.toSource();
	}
	@Override
	public Precedence getPrecedence() {
		return this.value.getPrecedence();
	}

	@SuppressWarnings("null")
	@Override
	public String toString() {
		return this.value.toString();
	}
	@Override
	public int getOffsetInParent() {
		return getOffset() + this.value.getOffsetInParent();
	}
	@Override
	public Class<? extends Expr> getExprClass() {
		return this.value.getExprClass();
	}

}
