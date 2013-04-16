package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;

public class Whitespace extends AbstractAtom implements HasFileRange, Token {
	public Whitespace(FileRange range) {
		super(range);
	}

	@Override
	public Expr transform(ExprTransformer transformer) {
		FileRange newRange = transformer.transform(this.fileRange);
		if(newRange == fileRange)
			return this;
		return new Whitespace(newRange);
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(' ');
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.lowest();
	}

	@Override
	public @Nullable <T> T acceptVisitor(TokenVisitor<T> visitor) {
		return visitor.visitWhitespace(this);
	}
}
