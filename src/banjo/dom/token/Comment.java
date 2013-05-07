package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.AbstractSourceNode;


public class Comment extends AbstractSourceNode implements Token {
	private final String text;

	public Comment(int sourceLength, String body) {
		super(sourceLength, body.hashCode());
		this.text = body;
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
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Comment))
			return false;
		final Comment other = (Comment) obj;
		if (!this.text.equals(other.text))
			return false;
		return true;
	}
}
