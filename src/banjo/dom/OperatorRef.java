package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;

public class OperatorRef extends AbstractExpr implements Atom {

	private final String op;

	public OperatorRef(FileRange range, String op) {
		super(range);
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
		return op;
	}

	@Override
	public Expr transform(ExprTransformer transformer) {
		FileRange newRange = transformer.transform(fileRange);
		if(newRange == fileRange)
			return this;
		return new Identifier(newRange, op);
	}
	
	@Override
	public @Nullable <T> T acceptVisitor(TokenVisitor<T> visitor) {
		return visitor.visitOperator(this);
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitOperator(this);
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.visitOperator(this);
	}
	
}
