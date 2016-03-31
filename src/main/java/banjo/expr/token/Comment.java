package banjo.expr.token;

import banjo.expr.util.FileRange;


public class Comment implements Token {
	private final FileRange fileRange;
	private final String text;

	public Comment(FileRange fileRange, String body) {
		this.fileRange = fileRange;
		this.text = body;

	}

	public FileRange getFileRange() {
		return fileRange;
	}

	public String getText() {
		return this.text;
	}

	@Override
	public String toString() {
		return this.text;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(this.text);
	}

	@Override
	public String toSource() {
		return this.text;
	}

	@Override
	public <T> T acceptVisitor(TokenVisitor<T> parser) {
		return parser.comment(fileRange, text);
	}
}
