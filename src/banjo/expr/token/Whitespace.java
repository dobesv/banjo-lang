package banjo.expr.token;

import banjo.expr.util.SourceFileRange;


public class Whitespace extends AbstractAtom implements Token {
	private final String text;

	public Whitespace(SourceFileRange sfr, String text) {
		super(sfr);
		this.text = text;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(this.text);
	}

	public String getText() {
		return this.text;
	}

	@Override
	public <T> T acceptVisitor(TokenVisitor<T> parser) {
		return parser.whitespace(getSourceFileRanges().head().getFileRange(), text);
	}

}
