package banjo.dom.token;

import banjo.dom.BadExpr;
import banjo.dom.core.CoreExprAlgebra;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprAlgebra;
import banjo.dom.source.SourceExprVisitor;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.data.List;

public class OperatorRef extends AbstractAtom implements Atom {

	public static final OperatorRef NONE = null;

	public static final Ord<OperatorRef> ORD = Ord.stringOrd.comap((OperatorRef opRef) -> opRef.op);

	private final String op;

	public OperatorRef(List<SourceFileRange> ranges, String op) {
		super(ranges);
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
		throw new Error("OperatorRef is not really a CoreExpr");
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.operator(this);
	}

	@Override
	public <T> T acceptVisitor(TokenVisitor<T> parser) {
		return parser.operator(getSourceFileRanges().head().getFileRange(), op);
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
