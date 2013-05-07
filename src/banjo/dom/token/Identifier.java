package banjo.dom.token;


import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprVisitor;
import banjo.parser.BanjoScanner;

public class Identifier extends AbstractAtom implements Atom, Key, Token {
	final String id;

	public Identifier(int sourceLength, String id) {
		super(sourceLength, id.hashCode());
		this.id = id;
	}

	public Identifier(String id) {
		super(NOT_FROM_SOURCE, id.hashCode());
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

}

