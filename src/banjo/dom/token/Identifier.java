package banjo.dom.token;


import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.dom.Expr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprVisitor;
import banjo.parser.BanjoScanner;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class Identifier extends AbstractAtom implements Atom, Key, Token {
	public static final Identifier ZERO = new Identifier(SourceFileRange.SYNTHETIC, "0");
	public static final Identifier EMPTY_STRING = new Identifier(SourceFileRange.SYNTHETIC, "\"\"");
	public static final Identifier EMPTY_LIST = new Identifier(SourceFileRange.SYNTHETIC, "[]");

	final String id;

	public Identifier(List<SourceFileRange> ranges, String id) {
		super(id.hashCode() + ranges.hashCode(), ranges);
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
		int spaces=0;
		for(int i=0; i < this.id.length(); i++) {
			final int cp = this.id.codePointAt(i);
			if(cp > Character.MAX_VALUE) i++;
			if(cp == ' ') spaces++;
			else spaces=0;
			final boolean ok = i==0?
					BanjoScanner.isIdentifierStart(cp):
						BanjoScanner.isIdentifierPart(cp) || (cp == ' ' && spaces == 1);
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
	public int compareTo(@Nullable Expr o) {
		if(o == null) return -1;
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final Identifier other = (Identifier) o;
			if(cmp == 0) cmp = this.id.compareTo(other.id);
		}
		return cmp;
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.nil();
	}
}

