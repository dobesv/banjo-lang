package banjo.dom.token;


import banjo.dom.BadExpr;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprAlgebra;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.core.ListLiteral;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprAlgebra;
import banjo.dom.source.SourceExprVisitor;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.data.List;

public class StringLiteral extends AbstractAtom implements Atom {
	public static final Ord<StringLiteral> ORD = Ord.stringOrd.comap((StringLiteral x) -> x.string);
	public final String string;

	public StringLiteral(List<SourceFileRange> ranges, String string) {
		super(ranges);
		this.string = string;
	}

	public StringLiteral(SourceFileRange range, String string) {
		this(List.single(range), string);
	}
	public StringLiteral(String string) {
		this(List.nil(), string);
	}

	public String getString() {
		return this.string;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(toSource(this.string));
	}

	public static String toSource(String text) {
		return toSource(text, new StringBuffer(text.length()+10)).toString();
	}

	public static StringBuffer toSource(String text, StringBuffer sb) {
		sb.append('"');
		for(int i=0; i < text.length(); i++) {
			final int cp = text.codePointAt(i);
			if(cp > Character.MAX_VALUE) i++; // Pair
			switch(cp) {
			case '\n': sb.append("\\n"); break;
			case '\r': sb.append("\\r"); break;
			case '\t': sb.append("\\t"); break;
			case '\f': sb.append("\\f"); break;
			case '\\': sb.append("\\\\"); break;
			case '"': sb.append("\\\""); break;
			default:
				sb.appendCodePoint(cp);
				break;
			}
		}
		sb.append('"');
		return sb;
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.stringLiteral(this);
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.stringLiteral(this);
	}

	@Override
	public <T> T acceptVisitor(TokenVisitor<T> parser) {
		return parser.stringLiteral(getSourceFileRanges().head().getFileRange(), string);
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.nil();
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.stringLiteral(getSourceFileRanges(), string);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.stringLiteral(getSourceFileRanges(), string);
	}

	public CoreExpr toConstructionExpression() {
		ListLiteral codePoints = new ListLiteral(
				List.list(getString().codePoints().mapToObj(cp -> new NumberLiteral(cp)).toArray(CoreExpr[]::new))
		);
		return Call.slot(Identifier.EMPTY_STRING, "with code points", List.single(codePoints));
	}

}
