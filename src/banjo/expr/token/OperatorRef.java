package banjo.expr.token;

import banjo.expr.BadExpr;
import banjo.expr.core.CoreExprAlgebra;
import banjo.expr.core.CoreExprVisitor;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExprAlgebra;
import banjo.expr.source.SourceExprVisitor;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;

public class OperatorRef extends AbstractAtom implements Atom {

	public static final OperatorRef NONE = null;

	public static final Ord<OperatorRef> ORD = Ord.stringOrd.contramap((OperatorRef opRef) -> opRef.op);

	public final String op;
	public final int indentColumn;

	public OperatorRef(List<SourceFileRange> ranges, int indentColumn, String op) {
		super(ranges, indentColumn);
		this.op = op;
		this.indentColumn = indentColumn;
	}

	public OperatorRef(SourceFileRange operatorRange, int indentColumn, String op) {
		this(List.single(operatorRange), indentColumn, op);
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
		throw new Error("OperatorRef is not really a CoreExpr");
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.operator(this);
	}

	@Override
	public <T> T acceptVisitor(TokenVisitor<T> parser) {
		return parser.operator(getSourceFileRanges().head().getFileRange(), 0, op);
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
