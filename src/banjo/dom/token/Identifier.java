package banjo.dom.token;


import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprVisitor;
import banjo.parser.BanjoScanner;

public class Identifier extends AbstractAtom implements Atom, Key, Token {
	final String id;

	public Identifier(String id) {
		super(id.hashCode());
		this.id = id;
	}

	public String getId() {
		return this.id;
	}
	@Override
	public String getKeyString() {
		return this.id;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public void toSource(StringBuffer sb) {
		for(int i=0; i < this.id.length(); i++) {
			final int cp = this.id.codePointAt(i);
			if(cp > Character.MAX_VALUE) i++;
			final boolean ok = i==0?
					BanjoScanner.isIdentifierStart(cp):
						BanjoScanner.isIdentifierPart(cp);
					if(!ok) {
						sb.append('\\');
					}
					sb.appendCodePoint(cp);
		}
	}

	@Override
	public @Nullable <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.identifier(this);
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.identifier(this);
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		final Identifier other = (Identifier) obj;
		if (other == null || !this.id.equals(other.id))
			return false;
		return true;
	}

	@Override
	public int compareTo(Expr o) {
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final Identifier other = (Identifier) o;
			if(cmp == 0) cmp = this.id.compareTo(other.id);
		}
		return cmp;
	}

}

