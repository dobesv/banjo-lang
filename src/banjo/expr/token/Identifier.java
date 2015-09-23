package banjo.expr.token;


import static java.util.Objects.requireNonNull;

import banjo.expr.BadExpr;
import banjo.expr.core.CoreExprAlgebra;
import banjo.expr.core.CoreExprVisitor;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExprAlgebra;
import banjo.expr.source.SourceExprVisitor;
import banjo.expr.util.FileRange;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;
import fj.data.Set;

public class Identifier extends AbstractAtom implements Atom, Token {
	public static final Ord<Identifier> ORD = Ord.stringOrd.contramap(x -> x.id);
	public static final Ord<List<Identifier>> LIST_ORD = Ord.listOrd(ORD);

	public static final Identifier ZERO = new Identifier("0");
	public static final Identifier ONE = new Identifier("1");
	public static final Identifier NAN = new Identifier("NaN");
	public static final Identifier EMPTY_STRING = new Identifier("empty string");
	public static final Identifier EMPTY_LIST = new Identifier("[]");
	public static final Identifier ENVIRONMENT = new Identifier("ε");
	public static final Identifier TRUE = new Identifier("true");
	public static final Identifier FALSE = new Identifier("false");
	public static final Identifier DATA = new Identifier("data");
	public static final Identifier INFINITY = new Identifier("∞");
	public static final Identifier UNDERSCORE = new Identifier("_");
	public static final Identifier __TMP = new Identifier("__tmp");
	public static final Identifier USAGE_EXAMPLES = new Identifier("usage examples");

	public final String id;

	public Identifier(Set<SourceFileRange> ranges, int indentColumn, String id) {
		super(ranges, indentColumn);
		this.id = requireNonNull(id);
	}

	public Identifier(SourceFileRange range, int indentColumn, String id) {
		this(Set.single(SourceFileRange.ORD, range), indentColumn, id);
	}

	public Identifier(String id) {
		this(SourceFileRange.EMPTY_SET, 0, id);
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
		int spaces = 0;
		for (int i = 0; i < id.length(); i++) {
			final int cp = id.codePointAt(i);
			if (cp > Character.MAX_VALUE) i++;
			if (cp == ' ') spaces++;
			else spaces = 0;
			final boolean ok = i == 0 ?
					TokenScanner.isIdentifierStart(cp) :
			        TokenScanner.isIdentifierPart(cp)
			                || (cp == ' ' && spaces == 1 && TokenScanner.isIdentifierStart(id.codePointAt(i+1)));
			if (!ok) {
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
		// Note that we are assuming this HAS a file range at all ...
		FileRange fileRange = getSourceFileRanges().toStream().head().getFileRange();
		return parser.identifier(fileRange, indentColumn, id);
	}
}

