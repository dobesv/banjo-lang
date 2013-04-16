package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;

public class Ellipsis extends AbstractAtom implements SourceExpr {

	public Ellipsis(FileRange range) {
		super(range);
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append("...");
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public Expr transform(ExprTransformer transformer) {
		FileRange newRange = transformer.transform(this.fileRange);
		if(newRange == this.fileRange) return this;
		return new Ellipsis(newRange);
	}
	
	@Override
	public @Nullable <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.visitEllipsis(this);
	}

}
