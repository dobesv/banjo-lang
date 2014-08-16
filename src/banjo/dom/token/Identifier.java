package banjo.dom.token;


import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.dom.Expr;
import banjo.dom.core.CoreExprAlgebra;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprAlgebra;
import banjo.dom.source.SourceExprVisitor;
import banjo.parser.SourceCodeScanner;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class Identifier extends AbstractAtom implements Atom, Key, Token {
	public static final Identifier ZERO = new Identifier("0");
	public static final Identifier EMPTY_STRING = new Identifier("\"\"");
	public static final Identifier EMPTY_LIST = new Identifier("[]");
	public static final Identifier ENVIRONMENT = new Identifier("ε");
	public static final Identifier TRUE = new Identifier("true");

	final String id;

	public Identifier(List<SourceFileRange> ranges, String id) {
		super(id.hashCode() + ranges.hashCode(), ranges);
		this.id = id;
	}

	public Identifier(SourceFileRange range, String id) {
		this(List.single(range), id);
	}

	public Identifier(String id) {
		this(List.<SourceFileRange>nil(), id);
	}

	public String getId() {
		return this.id;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public void toSource(StringBuffer sb) {
		toSource(id, sb);
	}

	public static void toSource(String id, StringBuffer sb) {
		int spaces=0;
		for(int i=0; i < id.length(); i++) {
			final int cp = id.codePointAt(i);
			if(cp > Character.MAX_VALUE) i++;
			if(cp == ' ') spaces++;
			else spaces=0;
			final boolean ok = i==0?
					SourceCodeScanner.isIdentifierStart(cp):
						SourceCodeScanner.isIdentifierPart(cp) || (cp == ' ' && spaces == 1);
					if(!ok) {
						sb.append('\\');
					}
					sb.appendCodePoint(cp);
		}
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.identifier(this);
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
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

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.identifier(getSourceFileRanges(), id);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.identifier(getSourceFileRanges(), id);
	}

	@Override
	public <T> T acceptVisitor(TokenVisitor<T> parser) {
		return parser.identifier(getSourceFileRanges().head().getFileRange(), id);
	}

	@Override
	public List<String> getParts() {
		return List.single(id);
	}


}

