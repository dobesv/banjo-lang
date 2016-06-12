package banjo.expr.token;


import static java.util.Objects.requireNonNull;

import banjo.eval.resolver.InstanceAlgebra;
import banjo.eval.resolver.NameRef;
import banjo.eval.resolver.NameRefAlgebra;
import banjo.eval.resolver.Resolver;
import banjo.expr.BadExpr;
import banjo.expr.core.CoreExprAlgebra;
import banjo.expr.core.CoreExprVisitor;
import banjo.expr.free.FreeExpression;
import banjo.expr.free.FreeExpressionVisitor;
import banjo.expr.free.PartialResolver;
import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExprAlgebra;
import banjo.expr.source.SourceExprVisitor;
import banjo.expr.util.FileRange;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class Identifier extends AbstractAtom implements Atom, Token, FreeExpression, NameRef {
	public static final Ord<Identifier> ORD = Ord.stringOrd.contramap(x -> x.id);
	public static final Ord<List<Identifier>> LIST_ORD = Ord.listOrd(ORD);

	public static final Identifier ZERO = new Identifier("0");
	public static final Identifier ONE = new Identifier("1");
	public static final Identifier NAN = new Identifier("NaN");
    public static final Identifier EMPTY_STRING = new Identifier("\"\"");
    public static final Identifier EMPTY_LIST = new Identifier("empty list");
    public static final Identifier SINGLE_ELEMENT_LIST = new Identifier("single element list");
    public static final Identifier PROJECT_ROOT = new Identifier("__project_root");
	public static final Identifier TRUE = new Identifier("true");
	public static final Identifier FALSE = new Identifier("false");
    public static final Identifier LABEL = new Identifier("0");
    public static final Identifier INFINITY = new Identifier("∞");
    public static final Identifier NEGATIVE_INFINITY = new Identifier("-∞");
	public static final Identifier UNDERSCORE = new Identifier("_");
	public static final Identifier __TMP = new Identifier("__tmp");
	public static final Identifier USAGE_EXAMPLES = new Identifier("usage examples");
    public static final Identifier LANGUAGE_KERNEL = new Identifier("language kernel");
    public static final Identifier LANGUAGE_KERNEL_STRING = new Identifier("language kernel string");
    public static final Identifier LANGUAGE_KERNEL_NUMBER = new Identifier("language kernel number");
    public static final Identifier MIRROR = new Identifier("mirror");
    public static final Identifier JAVA = new Identifier("java");
    public static final Identifier NUMBER = new Identifier("number");
    public static final Identifier STRING = new Identifier("string");
    public static final Identifier LIST = new Identifier("list");
    public static final Identifier EMPTY = new Identifier("empty");
    public static final Identifier SINGLETON = new Identifier("singleton");
    public static final Identifier FUNCTION_TRAIT = new Identifier("function trait");
    public static final Identifier SLOT_MAPPER = new Identifier("slot mapper");
    public static final Identifier FAIL = new Identifier("fail");
    public static final Identifier DYNAMIC_SLOT_PROXY = new Identifier("dynamic slot proxy");
    public static final Identifier DYNAMIC_CALL_PROXY = new Identifier("dynamic call proxy");
    public static final Identifier ARG_MAPPER = new Identifier("arg mapper");
    public static final Identifier KERNEL_STRING_VALUE = new Identifier("kernel string value");
    public static final Identifier KERNEL_NUMBER_VALUE = new Identifier("kernel number value");
    public static final Identifier KERNEL_BOOLEAN_VALUE = new Identifier("kernel boolean value");
    public static final Identifier INT8 = new Identifier("int8");
    public static final Identifier INT16 = new Identifier("int16");
    public static final Identifier INT32 = new Identifier("int32");
    public static final Identifier INT64 = new Identifier("int64");
    public static final Identifier INTEGER = new Identifier("integer");
    public static final Identifier FLOAT32 = new Identifier("float32");
    public static final Identifier FLOAT64 = new Identifier("float64");
    public static final Identifier DECIMAL = new Identifier("decimal");
    public static final Identifier FRACTION = new Identifier("fraction");
    public static final Identifier IS_INT8 = new Identifier("is int8");
    public static final Identifier IS_INT16 = new Identifier("is int16");
    public static final Identifier IS_INT32 = new Identifier("is int32");
    public static final Identifier IS_INT64 = new Identifier("is int64");
    public static final Identifier IS_INTEGER = new Identifier("is integer");
    public static final Identifier IS_FLOAT32 = new Identifier("is float32");
    public static final Identifier IS_FLOAT64 = new Identifier("is float64");
    public static final Identifier IS_DECIMAL = new Identifier("is decimal");
    public static final Identifier IS_FRACTION = new Identifier("is fraction");
    public static final Identifier BASE_FUNCTION_OPERATOR = new Identifier(Operator.BASE_FUNCTION.getOp());
    public static final Identifier FUNCTION_COMPOSE_OPERATOR = new Identifier(Operator.FUNCTION_COMPOSITION_LEFT.methodName);
    public static final Identifier EXTEND_OPERATOR = new Identifier(Operator.EXTENSION.getOp());
    public static final Identifier UNION_OPERATOR = new Identifier(Operator.UNION.methodName);
    public static final Identifier TYPE_UNION = new Identifier("type union");
    public static final Identifier COMPOSE = new Identifier("compose");

    public final String id;

	public Identifier(Set<SourceFileRange> ranges, int indentColumn, String id) {
		super(ranges, indentColumn);
		this.id = requireNonNull(id);
	}

    public Identifier(Set<SourceFileRange> ranges, String id) {
        this(ranges, 0, id);
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

    /**
     * Format a java string as an identifier, with any special characters
     * escaped, and append that to the given StringBuffer.
     */
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

    /**
     * Format a java string as an identifier, with any special characters
     * escaped.
     */
    public static String toSource(String id) {
        StringBuffer buf = new StringBuffer(id.length() + 4);
        toSource(id, buf);
        return buf.toString();
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
		return visitor.identifier(getRanges(), id);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.identifier(getRanges(), id);
	}

	@Override
	public <T> T acceptVisitor(TokenVisitor<T> parser) {
		// Note that we are assuming this HAS a file range at all ...
		FileRange fileRange = getRanges().toStream().head().getFileRange();
		return parser.identifier(fileRange, indentColumn, id);
	}

    @Override
    public <T> T acceptVisitor(FreeExpressionVisitor<T> visitor) {
        return visitor.identifier(this);
    }

    @Override
    public Set<NameRef> getFreeRefs() {
        return Set.single(NameRef.ORD, this);
    }

    @Override
    public boolean hasFreeRefs() {
        return true;
    }

    @Override
    public Option<FreeExpression> partial(PartialResolver resolver) {
        return resolver.local(ranges, id);
    }

    @Override
    public <T> T eval(List<T> trace, Resolver<T> resolver, InstanceAlgebra<T> algebra) {
        return resolver.local(ranges, id);
    }

    @Override
    public <T> T acceptVisitor(NameRefAlgebra<T> visitor) {
        return visitor.local(ranges, id);
    }
}

