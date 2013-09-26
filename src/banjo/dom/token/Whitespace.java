package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

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

	@Override
	public boolean equals(@Nullable Object obj) {
		if(obj == this) return true;
		if(obj == null || !(obj instanceof Whitespace)) return false;
		if(obj.hashCode() != this.hashCode()) return false;
		final Whitespace x = (Whitespace) obj;
		return x.text.equals(this.text);
	}

}
