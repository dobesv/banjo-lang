package banjo.dom.source;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

public class SourceExprTreeFold<T extends SourceExprTreeFold<T>> extends BaseSourceExprVisitor<T> {

	@SuppressWarnings("unchecked")
	T self = (T) this;

	@Override
	@Nullable
	public T binaryOp(BinaryOp op) {
		final T v1 = nonNull(op.getRight().acceptVisitor(this));
		final T v2 = nonNull(op.getLeft().acceptVisitor(v1));
		return v2;
	}

	@Override
	@Nullable
	public T unaryOp(UnaryOp op) {
		return nonNull(op.getOperand().acceptVisitor(this));
	}

	@Override
	@Nullable
	public T fallback(SourceExpr other) {
		return this.self;
	}

}
