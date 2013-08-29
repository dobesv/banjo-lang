package banjo.dom.token;

import banjo.dom.Expr;


public class Whitespace extends AbstractAtom implements Token {
	private final String text;

	public Whitespace(String text) {
		super(text.hashCode());
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
	public int compareTo(Expr o) {
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final Whitespace other = (Whitespace) o;
			cmp = this.text.compareTo(other.text);
		}
		return cmp;
	}

}
