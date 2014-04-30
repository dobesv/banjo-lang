package banjo.dom.source;

import static banjo.dom.Consts.CODEPOINT_NONE;
import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.ParenType;

public enum Operator {
	// Nullary operator
	EMPTY("", OperatorType.BUILTIN, Position.NA, Precedence.ATOM),

	// Unary operators
	PLUS("+", OperatorType.METHOD, Position.PREFIX, Precedence.UNARY_PREFIX),
	NEGATE("-", OperatorType.METHOD, Position.PREFIX, Precedence.UNARY_PREFIX),
	COMPLEMENT("~", OperatorType.METHOD, Position.PREFIX, Precedence.UNARY_PREFIX),
	NOT("!", OperatorType.METHOD, Position.PREFIX, Precedence.UNARY_PREFIX),
	LIST_ELEMENT("*", 0x2022, OperatorType.BUILTIN, Position.PREFIX, Precedence.BULLET),
	LAZY("->", 0x21a6, OperatorType.BUILTIN, Position.PREFIX, Precedence.FUNCTION),
	TABLE_HEADER("#::", OperatorType.BUILTIN, Position.PREFIX, Precedence.BULLET),
	TABLE_ROW(":::", OperatorType.BUILTIN, Position.PREFIX, Precedence.BULLET),
	PARENS(ParenType.PARENS, OperatorType.BUILTIN, Position.PREFIX),
	BRACKETS(ParenType.BRACKETS, OperatorType.BUILTIN, Position.PREFIX),
	ABSVALUE(ParenType.ABSVALUE, OperatorType.METHOD, Position.PREFIX),
	RETURN("^", 0x2191, OperatorType.BUILTIN, Position.PREFIX, Precedence.ASSIGNMENT), // Basically a unary parenthesis
	OBJECT_LITERAL(ParenType.BRACES, OperatorType.BUILTIN, Position.PREFIX),
	INSPECT("$", OperatorType.BUILTIN, Precedence.UNARY_PREFIX, Position.PREFIX),
	OPTIONAL("?", OperatorType.BUILTIN, Precedence.SUFFIX, Position.SUFFIX),
	EXISTS("??", OperatorType.BUILTIN, Precedence.SUFFIX, Position.SUFFIX),
	UNARY_NEWLINE_INDENT("(nl+indent)", OperatorType.BUILTIN, Position.PREFIX, Precedence.SEMICOLON),

	// Binary operators
	LOOKUP(ParenType.BRACKETS, OperatorType.METHOD, Position.INFIX),
	CALL(ParenType.PARENS, OperatorType.BUILTIN, Position.INFIX),
	EXTEND("@", 0x03A6, OperatorType.BUILTIN, Position.INFIX, Precedence.FUNCTION),
	PROJECTION(".", OperatorType.BUILTIN, Position.INFIX, Precedence.SUFFIX),
	OPT_PROJECTION(".?", OperatorType.BUILTIN, Position.INFIX, Precedence.SUFFIX),
	MAP_PROJECTION("*.", OperatorType.BUILTIN, Position.INFIX, Precedence.SUFFIX),
	MAP_OPT_PROJECTION("*.?", OperatorType.BUILTIN, Position.INFIX, Precedence.SUFFIX),
	CALL_NEXT_METHOD("@", OperatorType.BUILTIN, Position.PREFIX, Precedence.UNARY_PREFIX),
	QUICK_LAMBDA("&", OperatorType.BUILTIN, Position.PREFIX, Precedence.UNARY_PREFIX),
	MATCH("#", OperatorType.BUILTIN, Position.INFIX, Precedence.FUNCTION),
	POW("^", OperatorType.METHOD, Position.INFIX, Precedence.MULDIV),
	MUL("*", 0x00D7, OperatorType.METHOD, Position.INFIX, Precedence.MULDIV),
	DIV("/", 0x00F7, OperatorType.METHOD, Position.INFIX, Precedence.MULDIV),
	ADD("+", OperatorType.METHOD, Position.INFIX, Precedence.ADDSUB),
	SUB("-", OperatorType.METHOD, Position.INFIX, Precedence.ADDSUB),
	GT(">", OperatorType.METHOD, Position.INFIX, Precedence.ORDERING),
	GE(">=", 0x2265, OperatorType.METHOD, Position.INFIX, Precedence.ORDERING),
	LT("<", OperatorType.METHOD, Position.INFIX, Precedence.ORDERING),
	LE("<=", 0x2264, OperatorType.METHOD, Position.INFIX, Precedence.ORDERING),
	EQ("==", OperatorType.METHOD, Position.INFIX, Precedence.EQUALITY),
	NEQ("!=", 0x2260, OperatorType.METHOD, Position.INFIX, Precedence.EQUALITY),
	CMP("<=>", OperatorType.METHOD, Position.INFIX, Precedence.EQUALITY),
	MEMBER_OF("in", 0x2208, OperatorType.METHOD, null, Position.INFIX, Associativity.NA, Precedence.MEMBER_OF, Precedence.MEMBER_OF, "contains"),
	INTERSECT("&", 0x2229, OperatorType.METHOD, Position.INFIX, Precedence.INTERSECT),
	XOR("><", 0x22BB, OperatorType.METHOD, Position.INFIX, Precedence.XOR),
	UNION("|", 0x222A, OperatorType.METHOD, Position.INFIX, Precedence.UNION),
	LAZY_AND("&&", 0x2227, OperatorType.LAZY_RHS, Position.INFIX, Precedence.LAZY_AND),
	LAZY_OR("||", 0x2228, OperatorType.LAZY_RHS, Position.INFIX, Precedence.LAZY_OR),
	FUNCTION("->", 0x21A6, OperatorType.BUILTIN, Position.INFIX, Precedence.FUNCTION, Associativity.RIGHT),
	ASSIGNMENT("=", OperatorType.BUILTIN, Position.INFIX, Precedence.ASSIGNMENT, Associativity.RIGHT),
	MONAD_EXTRACT("<-", OperatorType.BUILTIN, Position.INFIX, Precedence.ASSIGNMENT, Associativity.RIGHT),
	CONS("::", OperatorType.METHOD_SWITCHED, Position.INFIX, Precedence.COLON, Associativity.RIGHT),
	COND("=>", 0x21D2, OperatorType.LAZY_RHS, Position.INFIX, Precedence.LAZY_OR, Precedence.COND, Associativity.NA),
	COMMA(",", OperatorType.BUILTIN, Position.INFIX, Precedence.COMMA, Associativity.RIGHT),
	SEMICOLON(";", OperatorType.LAZY_RHS, Position.INFIX, Precedence.SEMICOLON, Associativity.RIGHT),
	NEWLINE("(nl)", OperatorType.BUILTIN, Position.INFIX, Precedence.SEMICOLON, Associativity.RIGHT),

	// Special case operators
	INVALID("~~~INVALID~~~", OperatorType.BUILTIN, Position.NA, Precedence.ATOM),
	MISSING("~~~MISSING~~~", OperatorType.BUILTIN, Position.NA, Precedence.ATOM); // Newline and indent

	public static enum Associativity { LEFT, RIGHT, NA; }
	public static enum Position { PREFIX, INFIX, SUFFIX, NA }
	private final String op;
	private final int codePoint; // -1 if no special unicode character
	private final Precedence leftPrecedence; // For binary operators only
	private final Precedence precedence;
	private final @Nullable ParenType parenType; // nullable
	private final Associativity associativity;
	private final Position position;
	private final String methodName;
	private final OperatorType operatorType;

	Operator(String op, int codePoint, OperatorType operatorType, @Nullable ParenType parenType, Position position, Associativity associativity, Precedence leftPrecedence, Precedence precedence, String methodName) {
		this.op = op;
		this.codePoint = codePoint;
		this.operatorType = operatorType;
		this.leftPrecedence = leftPrecedence;
		this.precedence = precedence;
		this.parenType = parenType;
		this.associativity = associativity;
		this.position = position;
		this.methodName = methodName;
	}

	Operator(String op, int codePoint, OperatorType operatorType, @Nullable ParenType parenType, Position position, Associativity associativity, Precedence leftPrecedence, Precedence precedence) {
		this(op, codePoint, operatorType, parenType, position, associativity, leftPrecedence, precedence,
				(position==Position.PREFIX || associativity==Associativity.RIGHT) && !op.endsWith(":") ? op+":" : op);
	}

	Operator(String op, int codePoint, OperatorType operatorType, @Nullable ParenType parenType, Position position, Associativity associativity, Precedence precedence) {
		this(op, codePoint, operatorType, parenType, position, associativity, precedence, precedence);
	}
	Operator(String op, int codePoint, OperatorType operatorType, ParenType parenType, Position position, Precedence p) {
		this(op, codePoint, operatorType, parenType, position, Associativity.NA, p);
	}
	Operator(String op, int codePoint, OperatorType operatorType, Position position, Precedence p) {
		this(op, codePoint, operatorType, null, position, defaultAssociativity(position), p);
	}

	public static Associativity defaultAssociativity(Position position) {
		switch(position) {
		case PREFIX: return Associativity.RIGHT;
		default:
		case INFIX:
		case SUFFIX: return Associativity.LEFT;
		}
	}

	Operator(String op, OperatorType operatorType, Position position, Precedence p) {
		this(op, CODEPOINT_NONE, operatorType, position, p);
	}
	Operator(String op, int cp, OperatorType operatorType, Position position, Precedence p, Associativity associativity) {
		this(op, cp, operatorType, null, position, associativity, p);
	}
	Operator(String op, OperatorType operatorType, Position position, Precedence p, Associativity associativity) {
		this(op, CODEPOINT_NONE, operatorType, position, p, associativity);
	}
	Operator(String op, int cp, OperatorType operatorType, Position position, Precedence left, Precedence p, Associativity associativity) {
		this(op, cp, operatorType, null, position, associativity, left, p);
	}
	Operator(String op, OperatorType operatorType, Position position, Precedence left, Precedence p, Associativity associativity) {
		this(op, CODEPOINT_NONE, operatorType, position, left, p, associativity);
	}
	Operator(String op, int codePoint, OperatorType operatorType, Precedence p, Associativity associativity) {
		this(op, codePoint, operatorType, null, Position.INFIX, associativity, p);
	}
	Operator(ParenType parenType, OperatorType operatorType, Position position) {
		this(parenType.getEmptyPairString(), CODEPOINT_NONE, operatorType, parenType, position, defaultAssociativity(position), Precedence.SUFFIX);
	}

	public static @Nullable Operator fromOp(String op, Position pos) {
		for(final Operator operator : values()) {
			// Wrong position
			if(operator.getPosition() != pos)
				continue;
			if(operator.isParen() && op.length() == 1) {
				if(operator.getParenType().getStartChar() == op.charAt(0))
					return operator;
			} else if(op.equals(operator.getOp()) || (operator.codePoint > 0 && op.length()==1 && op.codePointAt(0) == operator.codePoint)) {
				return operator;
			}
		}
		return null;
	}

	public static @Nullable Operator fromMethodName(String methodName) {
		for(final Operator operator : values()) {
			if(methodName.equals(operator.methodName)) {
				return operator;
			}
		}
		return null;
	}

	public static @Nullable Operator fromParenType(ParenType pt, Position pos) {
		for(final Operator operator : values()) {
			if(pt.equals(operator.parenType) && pos == operator.position) {
				return operator;
			}
		}
		return null;
	}
	public int getCodePoint() {
		return this.codePoint;
	}

	/**
	 * Get the type of paren this operator is associated with.  Fails if this is not associated
	 * with a paren.
	 */
	public ParenType getParenType() {
		return nonNull(this.parenType);
	}

	/**
	 * @return true if this operator is a parenthesis type operator
	 */
	public boolean isParen() {
		return this.parenType != null;
	}
	public Associativity getAssociativity() {
		return this.associativity;
	}

	public boolean isLeftAssociative() {
		return this.associativity == Associativity.LEFT;
	}

	public boolean isRightAssociative() {
		return this.associativity == Associativity.RIGHT;
	}



	Operator(String op, OperatorType operatorType, Precedence precedence, Position position) {
		this(op, CODEPOINT_NONE, operatorType, position, precedence);
	}

	public String getOp() {
		return this.op;
	}

	public Precedence getLeftPrecedence() {
		return this.leftPrecedence;
	}

	public Precedence getPrecedence() {
		return this.precedence;
	}
	public Position getPosition() {
		return this.position;
	}
	public boolean isSuffix() {
		return this.position == Position.SUFFIX;
	}
	public boolean isInfix() {
		return this.position == Position.INFIX;
	}
	public boolean isPrefix() {
		return this.position == Position.PREFIX;
	}

	public void toSource(StringBuffer sb) {
		sb.append(getOp());
	}

	public String getMethodName() {
		return this.methodName;
	}

	public OperatorType getOperatorType() {
		return this.operatorType;
	}

	public boolean isSelfOnRightMethodOperator() {
		return isPrefix() || (isInfix() && getOperatorType() == OperatorType.METHOD_SWITCHED);
	}
}