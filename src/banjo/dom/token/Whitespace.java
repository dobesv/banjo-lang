package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.parser.util.SourceFileRange;


public class Whitespace extends AbstractAtom implements Token {
	private final String text;

	public Whitespace(SourceFileRange sfr, String text) {
		super(text.hashCode(), sfr);
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

	@Override
	public int compareTo(@Nullable Expr o) {
		if(this == o)
			return 0;
		if(o == null) return -1;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final Whitespace other = (Whitespace) o;
			cmp = this.text.compareTo(other.text);
		}
		return cmp;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if(obj == this) return true;
		if(obj == null || !(obj instanceof Whitespace)) return false;
		if(obj.hashCode() != this.hashCode()) return false;
		final Whitespace x = (Whitespace) obj;
		return x.text.equals(this.text);
	}

}
