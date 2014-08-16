package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.AbstractCachedHashCode;
import banjo.parser.util.FileRange;


public class BadToken extends AbstractCachedHashCode implements Token {
	private final FileRange fileRange;
	private final String text;
	private final String message;

	public BadToken(FileRange fileRange, String text, String message) {
		super(text.hashCode() + message.hashCode());
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
	public boolean equals(@Nullable Object obj) {
		if(obj == this) return true;
		if(obj == null || !(obj instanceof BadToken)) return false;
		if(obj.hashCode() != this.hashCode()) return false;
		final BadToken x = (BadToken) obj;
		return x.message.equals(this.message) && x.text.equals(this.text) && fileRange.equals(x.fileRange);
	}

	@Override
	public <T> T acceptVisitor(TokenVisitor<T> parser) {
		return parser.badToken(getFileRange(), text, message);
	}

}
