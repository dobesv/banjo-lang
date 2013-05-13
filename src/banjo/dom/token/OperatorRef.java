package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprVisitor;

public class OperatorRef extends AbstractAtom implements Atom {

	public static final @Nullable OperatorRef NONE = null;

	private final String op;

	public OperatorRef(int sourceLength, String op) {
		super(sourceLength, op.hashCode());
		this.op = op;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(this.getOp());
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	public String getOp() {
		return this.op;
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.operator(this);
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.operator(this);
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof OperatorRef))
			return false;
		final OperatorRef other = (OperatorRef) obj;
		if (!this.op.equals(other.op))
			return false;
		return true;
	}

}