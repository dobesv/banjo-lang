package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.dom.Expr;
import banjo.dom.core.CoreExprAlgebra;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprAlgebra;
import banjo.dom.source.SourceExprVisitor;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class OperatorRef extends AbstractAtom implements Atom {

	public static final @Nullable OperatorRef NONE = null;

	private final String op;

	public OperatorRef(List<SourceFileRange> ranges, String op) {
		super(op.hashCode(), ranges);
		this.op = op;
	}

	public OperatorRef(SourceFileRange operatorRange, String op) {
		this(List.single(operatorRange), op);
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
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.operator(this);
	}

	@Override
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
	public int compareTo(@Nullable Expr o) {
		if(this == o)
			return 0;
		if(o == null) return -1;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final OperatorRef other = (OperatorRef) o;
			if(cmp == 0) cmp = this.op.compareTo(other.op);
		}
		return cmp;
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.nil();
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.identifier(getSourceFileRanges(), op);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.identifier(getSourceFileRanges(), op);
	}

}
