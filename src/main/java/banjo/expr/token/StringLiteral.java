package banjo.expr.token;


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

public class StringLiteral extends AbstractAtom implements Atom {
	public static final Ord<StringLiteral> ORD = Ord.stringOrd.contramap((StringLiteral x) -> x.string);
    public final String string;

    public StringLiteral(SourceFileRange range, int indentColumn, String string) {
        super(range, indentColumn);
        this.string = string;
    }

    public StringLiteral(Set<SourceFileRange> ranges, int indentColumn, String string) {
        super(ranges, indentColumn);
        this.string = string;
	}

    public StringLiteral(Set<SourceFileRange> ranges, String string) {
        this(ranges, 0, string);
    }

	public StringLiteral(String string) {
        this(SourceFileRange.EMPTY_SET, 0, string);
	}

    @Override
    public String toSource() {
        return toSource(this.string);
    }

    @Override
    public void toSource(StringBuffer sb) {
        toSource(this.string, sb);
    }

    public static String toSource(String text) {
        return toSource(text, new StringBuffer(text.length() + 10)).toString();
    }

    public static StringBuffer toSource(String text, StringBuffer sb) {
        sb.append('"');
        for (int i = 0; i < text.length(); i++) {
            final int cp = text.codePointAt(i);
            if (cp > Character.MAX_VALUE)
                i++; // Pair
            switch (cp) {
            case '\n':
                sb.append("\\n");
                break;
            case '\r':
                sb.append("\\r");
                break;
            case '\t':
                sb.append("\\t");
                break;
            case '\f':
                sb.append("\\f");
                break;
            case '\\':
                sb.append("\\\\");
                break;
            case '"':
                sb.append("\\\"");
                break;
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
        FileRange fileRange = getRanges().min().some().getFileRange();
        return parser.stringLiteral(fileRange, indentColumn, string);
	}

    @Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
        return visitor.stringLiteral(getRanges(), string);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.stringLiteral(getRanges(), string);
	}

    public String getString() {
    	return this.string;
    }

    @Override
    public Precedence getPrecedence() {
    	return Precedence.ATOM;
    }

    @Override
    public List<BadExpr> getProblems() {
    	return List.nil();
    }

}

