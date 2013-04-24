package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.FileRange;

public class BadExpr extends AbstractExpr implements CoreExpr, SourceExpr {
	private final BanjoParseException error;
	
	public BadExpr(FileRange range, BanjoParseException error) {
		super(range);
		this.error = error;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append("fail(");
		StringLiteral.toSource(error.toString(), sb);
		sb.append(")");
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.SUFFIX;
	}

	@Override
	public Expr transform(ExprTransformer transformer) {
		FileRange newRange = transformer.transform(fileRange);
		if(newRange == fileRange)
			return this;
		return new BadExpr(newRange, error);
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitBadExpr(this);
	}

	@Override
	@Nullable
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.visitBadExpr(this);
	}

}
