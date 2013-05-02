package banjo.dom.token;

import banjo.dom.source.AbstractSourceNode;
import banjo.dom.source.Precedence;

public class Whitespace extends AbstractSourceNode implements Token {
	public Whitespace(int sourceLength) {
		super(sourceLength);
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(' ');
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.lowest();
	}

}
