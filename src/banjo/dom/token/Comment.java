package banjo.dom.token;

import banjo.parser.util.AbstractCachedHashCode;
import banjo.parser.util.FileRange;


public class Comment extends AbstractCachedHashCode implements Token {
	private final FileRange fileRange;
	private final String text;

	public Comment(FileRange fileRange, String body) {
		super(body.hashCode());
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
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if(obj.hashCode() != this.hashCode())
			return false;
		if (!(obj instanceof Comment))
			return false;
		final Comment other = (Comment) obj;
		if (!this.text.equals(other.text))
			return false;
		if (!this.fileRange.equals(other.fileRange))
			return false;
		return true;
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
