package banjo.dom.source;

import static banjo.dom.Consts.CODEPOINT_NONE;
import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.ParenType;

public enum Operator {
	// Nullary operator
	EMPTY("", Position.NA, Precedence.ATOM),

	// Unary operators
	PLUS("+", Position.PREFIX, Precedence.UNARY_PREFIX),
	NEGATE("-", Position.PREFIX, Precedence.UNARY_PREFIX),
	COMPLEMENT("~", Position.PREFIX, Precedence.UNARY_PREFIX),
	NOT("!", Position.PREFIX, Precedence.UNARY_PREFIX),
	LIST_ELEMENT("*", 0x2022, Position.PREFIX, Precedence.BULLET),
	LAZY("->", 0x21a6, Position.PREFIX, Precedence.FUNCTION),
	TABLE_HEADER("#::", Position.PREFIX, Precedence.BULLET),
	TABLE_ROW("::", Position.PREFIX, Precedence.BULLET),
	PARENS(ParenType.PARENS, Position.PREFIX),
	BRACKETS(ParenType.BRACKETS, Position.PREFIX),
	RETURN("^", 0x2191, Position.PREFIX, Precedence.ASSIGNMENT), // Basically a unary parenthesis
	OBJECT_LITERAL(ParenType.BRACES, Position.PREFIX),
	INSPECT("#", Precedence.SUFFIX, Position.SUFFIX),
	OPTIONAL("?", Precedence.SUFFIX, Position.SUFFIX),
	EXISTS("??", Precedence.SUFFIX, Position.SUFFIX),
	UNARY_NEWLINE_INDENT("(nl+indent)", Position.PREFIX, Precedence.SEMICOLON),

	// Binary operators
	LOOKUP(ParenType.BRACKETS, Position.INFIX),
	CALL(ParenType.PARENS, Position.INFIX),
	EXTEND("{+}", 0x03A6, Position.INFIX, Precedence.FUNCTION),
	PROJECTION(".", Position.INFIX, Precedence.SUFFIX),
	OPT_PROJECTION(".?", Position.INFIX, Precedence.SUFFIX),
	MAP_PROJECTION("*.", Position.INFIX, Precedence.SUFFIX),
	MAP_OPT_PROJECTION("*.?", Position.INFIX, Precedence.SUFFIX),
	POW("^", Position.INFIX, Precedence.MULDIV),
	MUL("*", 0x00D7, Position.INFIX, Precedence.MULDIV),
	DIV("/", 0x00F7, Position.INFIX, Precedence.MULDIV),
	ADD("+", Position.INFIX, Precedence.ADDSUB),
	SUB("-", Position.INFIX, Precedence.ADDSUB),
	GT(">", Position.INFIX, Precedence.ORDERING),
	GE(">=", 0x2265, Position.INFIX, Precedence.ORDERING),
	LT("<", Position.INFIX, Precedence.ORDERING),
	LE("<=", 0x2264, Position.INFIX, Precedence.ORDERING),
	EQ("==", Position.INFIX, Precedence.EQUALITY),
	NEQ("!=", 0x2260, Position.INFIX, Precedence.EQUALITY),
	CMP("<=>", Position.INFIX, Precedence.EQUALITY),
	INTERSECT("&", 0x2229, Position.INFIX, Precedence.INTERSECT),
	XOR("><", 0x22BB, Position.INFIX, Precedence.XOR),
	UNION("|", 0x222A, Position.INFIX, Precedence.UNION),
	LAZY_AND("&&", 0x2227, Position.INFIX, Precedence.LAZY_AND),
	LAZY_OR("||", 0x2228, Position.INFIX, Precedence.LAZY_OR),
	FUNCTION("->", 0x21A6, Position.INFIX, Precedence.FUNCTION, Associativity.RIGHT),
	ASSIGNMENT("=", Position.INFIX, Precedence.ASSIGNMENT, Associativity.RIGHT),
	MONAD_EXTRACT("<-", Position.INFIX, Precedence.ASSIGNMENT, Associativity.RIGHT),
	COLON(":", Position.INFIX, Precedence.COLON, Associativity.RIGHT),
	COND("=>", 0x21D2, Position.INFIX, Precedence.COND, Associativity.RIGHT),
	COMMA(",", Position.INFIX, Precedence.COMMA, Associativity.RIGHT),
	SEMICOLON(";", Position.INFIX, Precedence.SEMICOLON, Associativity.RIGHT),
	NEWLINE("(nl)", Position.INFIX, Precedence.SEMICOLON, Associativity.RIGHT),

	// Special case operators
	INVALID("~~~INVALID~~~", Position.NA, Precedence.ATOM),
	MISSING("~~~MISSING~~~", Position.NA, Precedence.ATOM); // Newline and indent

	public static enum Associativity { LEFT, RIGHT, NA; }
	public static enum Position { PREFIX, INFIX, SUFFIX, NA }
	private final String op;
	private final int codePoint; // -1 if no special unicode character
	private final Precedence precedence;
	private final @Nullable ParenType parenType; // nullable
	private final Associativity associativity;
	private final Position position;

	Operator(String op, int codePoint, @Nullable ParenType parenType, Position position, Associativity associativity, Precedence precedence) {
		this.op = op;
		this.codePoint = codePoint;
		this.precedence = precedence;
		this.parenType = parenType;
		this.associativity = associativity;
		this.position = position;
	}

	Operator(String op, int codePoint, ParenType parenType, Position position, Precedence p) {
		this(op, codePoint, parenType, position, Associativity.NA, p);
	}
	Operator(String op, int codePoint, Position position, Precedence p) {
		this(op, codePoint, null, position, defaultAssociativity(position), p);
	}

	public static Associativity defaultAssociativity(Position position) {
		switch(position) {
		case PREFIX: return Associativity.RIGHT;
		default:
		case INFIX:
		case SUFFIX: return Associativity.LEFT;
		}
	}

	Operator(String op, Position position, Precedence p) {
		this(op, CODEPOINT_NONE, position, p);
	}
	Operator(String op, int cp, Position position, Precedence p, Associativity associativity) {
		this(op, cp, null, position, associativity, p);
	}
	Operator(String op, Position position, Precedence p, Associativity associativity) {
		this(op, CODEPOINT_NONE, position, p, associativity);
	}
	Operator(String op, int codePoint, Precedence p, Associativity associativity) {
		this(op, codePoint, null, Position.INFIX, associativity, p);
	}
	Operator(ParenType parenType, Position position) {
		this(parenType.getEmptyPairString(), CODEPOINT_NONE, parenType, position, defaultAssociativity(position), Precedence.SUFFIX);
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



	Operator(String op, Precedence precedence, Position position) {
		this(op, CODEPOINT_NONE, position, precedence);
	}

	public String getOp() {
		return this.op;
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

}