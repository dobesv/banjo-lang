package banjo.expr.token;

import banjo.expr.util.FileRange;


public class BadToken implements Token {
	private final FileRange fileRange;
	private final String text;
	private final String message;

	public BadToken(FileRange fileRange, String text, String message) {
		this.fileRange = fileRange;
		this.text = text;
		this.message = message;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(this.text);
	}

	@Override
	public String toSource() {
		return this.text;
	}

	public FileRange getFileRange() {
		return fileRange;
	}

	public String getText() {
		return this.text;
	}

	public String getMessage() {
		return this.message;
	}

	@Override
	public <T> T acceptVisitor(TokenVisitor<T> parser) {
		return parser.badToken(getFileRange(), text, message);
	}

}
