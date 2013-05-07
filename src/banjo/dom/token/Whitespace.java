package banjo.dom.token;

import banjo.dom.source.AbstractSourceNode;

public class Whitespace extends AbstractSourceNode implements Token {
	private final String text;

	public Whitespace(int sourceLength, String text) {
		super(sourceLength, text.hashCode());
		this.text = text;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(this.text);
	}

	public String getText() {
		return this.text;
	}

}
