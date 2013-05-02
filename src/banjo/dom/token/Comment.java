package banjo.dom.token;

import banjo.dom.source.AbstractSourceNode;


public class Comment extends AbstractSourceNode implements Token {
	private final String text;

	public Comment(int sourceLength, String body) {
		super(sourceLength);
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



}
