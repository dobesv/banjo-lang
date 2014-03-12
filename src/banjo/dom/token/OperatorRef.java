package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprVisitor;
import banjo.parser.util.SourceFileRange;

public class OperatorRef extends AbstractAtom implements Atom {

	public static final @Nullable OperatorRef NONE = null;

	private final String op;

	public OperatorRef(SourceFileRange sfr, String op) {
		super(op.hashCode(), sfr);
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

	@Override
	public int compareTo(Expr o) {
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final OperatorRef other = (OperatorRef) o;
			if(cmp == 0) cmp = this.op.compareTo(other.op);
		}
		return cmp;
	}

}
