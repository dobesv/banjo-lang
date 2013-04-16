package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;

public class Comment extends AbstractExpr implements Token {
	private final String text;

	public Comment(FileRange range, String body) {
		super(range);
		this.text = body;
	}

	public String getText() {
		return text;
	}
	
	@Override
	public String toString() {
		return text;
	}

	@Override
	public @Nullable <T> T acceptVisitor(TokenVisitor<T> visitor) {
		return visitor.visitComment(this);
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(text);
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.lowest();
	}

	@Override
	public Expr transform(ExprTransformer transformer) {
		FileRange newRange = transformer.transform(fileRange);
		String newText = transformer.transform(text, fileRange);
		if(newRange == this.fileRange && newText == this.text)
			return this;
		return new Comment(newRange, newText);
	}
	
}
