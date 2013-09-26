package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.AbstractCachedHashCode;


public class BadToken extends AbstractCachedHashCode implements Token {
	private final String text;
	private final String message;

	public BadToken(String text, String message) {
		super(text.hashCode() + message.hashCode());
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
		return x.message.equals(this.message) && x.text.equals(this.text);
	}



}
