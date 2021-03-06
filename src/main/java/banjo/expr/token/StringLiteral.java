package banjo.expr.token;


import banjo.eval.resolver.GlobalRef;
import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.Resolver;
import banjo.expr.BadExpr;
import banjo.expr.core.CoreExprAlgebra;
import banjo.expr.core.CoreExprVisitor;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionVisitor;
import banjo.expr.free.PartialResolver;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExprAlgebra;
import banjo.expr.source.SourceExprVisitor;
import banjo.expr.util.FileRange;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class StringLiteral extends AbstractAtom implements Atom, FreeExpression {
	public static final Ord<StringLiteral> ORD = Ord.stringOrd.contramap((StringLiteral x) -> x.string);
	public final String string;
    public final boolean kernelString;

	public StringLiteral(Set<SourceFileRange> ranges, int indentColumn, String string, boolean kernelString) {
		super(ranges, indentColumn);
		this.string = string;
        this.kernelString = kernelString;
	}

    public StringLiteral(Set<SourceFileRange> ranges, String string, boolean kernelString) {
        this(ranges, 0, string, kernelString);
    }

    public StringLiteral(SourceFileRange range, int indentColumn, String string, boolean kernelString) {
		this(Set.single(SourceFileRange.ORD, range), indentColumn, string, kernelString);
	}
	public StringLiteral(String string) {
        this(SourceFileRange.EMPTY_SET, 0, string, false);
	}

    public StringLiteral(String string, boolean kernelString) {
        this(SourceFileRange.EMPTY_SET, 0, string, kernelString);
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
		FileRange fileRange = getRanges().toStream().head().getFileRange();
		return parser.stringLiteral(fileRange, indentColumn, string, kernelString);
	}

	@Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.stringLiteral(this);
    }

    @Override
	public List<BadExpr> getProblems() {
		return List.nil();
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.stringLiteral(getRanges(), string, kernelString);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.stringLiteral(getRanges(), string);
	}

    @Override
    public Set<NameRef> getFreeRefs() {
        return Set.set(NameRef.ORD, GlobalRef.TRUE, GlobalRef.LANGUAGE_KERNEL_STRING);
    }

    @Override
    public boolean hasFreeRefs() {
        return true;
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver resolver) {
        return Option.none();
    }

    @Override
    public <T> T eval(List<T> trace, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        T kernelString = algebra.kernelString(ranges, string, resolver.global(GlobalRef.TRUE));
        return algebra.call1(trace, ranges, resolver.global(GlobalRef.LANGUAGE_KERNEL_STRING), kernelString);
    }

    public StringLiteral toKernelString() {
        return new StringLiteral(ranges, indentColumn, string, true);
    }
}
