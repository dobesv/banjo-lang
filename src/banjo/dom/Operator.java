package banjo.dom;

import static banjo.dom.Consts.CODEPOINT_NONE;
import static banjo.dom.Consts.METHOD_NONE;
import static banjo.dom.Consts.PAREN_NONE;
import fj.data.Option;

public enum Operator {
	// Unary operators
	PLUS("+", Position.PREFIX, Precedence.UNARY_PREFIX, "plus"),
	NEGATE("-", Position.PREFIX, Precedence.UNARY_PREFIX, "negate"),
	COMPLEMENT("~", Position.PREFIX, Precedence.UNARY_PREFIX, "complement"),
	NOT("!", Position.PREFIX, Precedence.UNARY_PREFIX, "false"),
	MIRROR(":", Position.PREFIX, Precedence.UNARY_PREFIX),	
	LIST_ELEMENT("*", 0x2022, Position.PREFIX, Precedence.BULLET),
	SET_ELEMENT("|", Position.PREFIX, Precedence.BULLET),
	LAZY("->", 0x21a6, Position.PREFIX, Precedence.FUNCTION),
	TABLE_HEADER("#::", Position.PREFIX, Precedence.BULLET),
	TABLE_ROW("::", Position.PREFIX, Precedence.BULLET),
	PARENS(ParenType.PARENS, Position.PREFIX),
	LIST_LITERAL(ParenType.BRACKETS, Position.PREFIX),
	RETURN("^", 0x2191, Position.PREFIX, Precedence.ASSIGNMENT), // Basically a unary parenthesis
	OBJECT_OR_SET_LITERAL(ParenType.BRACES, Position.PREFIX),
	UNARY_CALL("()", Position.SUFFIX, Precedence.SUFFIX), // Unary call with no parameters, possibly reading a lazy value
	OPTIONAL("?", Precedence.SUFFIX, Position.SUFFIX, "asOptionalContract"),
	EXISTS("??", Precedence.SUFFIX, Position.SUFFIX, "hasValue"),
	NEWLINE_INDENT("\\n\\t", Position.PREFIX, Precedence.SEMICOLON),
	
	// Binary operators
	LOOKUP(ParenType.BRACKETS, Position.INFIX),
	CALL(ParenType.PARENS, Position.INFIX),
	PROJECTION(".", Position.INFIX, Precedence.SUFFIX),
	MAP_PROJECTION("?.", Position.INFIX, Precedence.SUFFIX),
	POW("^", Position.INFIX, Precedence.MULDIV, "toThePowerOf"),
	MUL("*", 0x00D7, Position.INFIX, Precedence.MULDIV, "times"),
	DIV("/", 0x00F7, Position.INFIX, Precedence.MULDIV, "dividedBy"),
	ADD("+", Position.INFIX, Precedence.ADDSUB, "plus"),
	SUB("-", Position.INFIX, Precedence.ADDSUB, "minus"),
	GT(">", Position.INFIX, Precedence.ORDERING),
	GE(">=", 0x2265, Position.INFIX, Precedence.ORDERING),
	LT("<", Position.INFIX, Precedence.ORDERING),
	LE("<=", 0x2264, Position.INFIX, Precedence.ORDERING),
	EQ("==", Position.INFIX, Precedence.EQUALITY, "eq"),
	NEQ("!=", 0x2260, Position.INFIX, Precedence.EQUALITY),
	CMP("<=>", Position.INFIX, Precedence.EQUALITY, "cmp"),
	INTERSECT("&", 0x2229, Position.INFIX, Precedence.INTERSECT, "intersection"),
	XOR("><", 0x22BB, Position.INFIX, Precedence.XOR, "xor"),
	UNION("|", 0x222A, Position.INFIX, Precedence.UNION, "union"),
	LAZY_AND("&&", 0x2227, Position.INFIX, Precedence.LAZY_AND),
	LAZY_OR("||", 0x2228, Position.INFIX, Precedence.LAZY_OR),
	COMMA(",", Position.INFIX, Precedence.COMMA),
	FUNCTION("->", 0x21A6, Position.INFIX, Precedence.FUNCTION, Associativity.RIGHT),
	ASSIGNMENT("=", Position.INFIX, Precedence.ASSIGNMENT, Associativity.RIGHT),
	PAIR(":", Position.INFIX, Precedence.COLON, Associativity.RIGHT),
	COND("=>", 0x21D2, Position.INFIX, Precedence.COND, Associativity.RIGHT),
	OR_ELSE("?:", Position.INFIX, Precedence.COND, Associativity.RIGHT),
	SEMICOLON(";", Position.INFIX, Precedence.SEMICOLON),
	NEWLINE("\\n", Position.INFIX, Precedence.SEMICOLON),
	
	// Special case operators
	INVALID("~~~INVALID~~~", Position.INFIX, Precedence.ATOM),
	MISSING("~~~MISSING~~~", Position.INFIX, Precedence.ATOM); // Newline and indent
	
	public static enum Associativity { LEFT, RIGHT, NA; }
	public static enum Position { PREFIX, INFIX, SUFFIX }
	private final String op;
	private final int codePoint; // -1 if no special unicode character
	private final Precedence precedence;
	private final Option<ParenType> parenType; // nullable
	private final Option<String> methodName; // nullable
	private final Associativity associativity;
	private final Position position;
	
	Operator(String op, int codePoint, Option<ParenType> parenType, Position position, Precedence precedence, Associativity associativity, Option<String> methodName) {
		this.op = op;
		this.codePoint = codePoint;
		this.precedence = precedence;
		this.parenType = parenType;
		this.methodName = methodName;
		this.associativity = associativity;
		this.position = position;
	}
	
	Operator(String op, int codePoint, ParenType parenType, Position position, Precedence p, String methodName) {
		this(op, codePoint, Option.some(parenType), position, p, Associativity.LEFT, Option.some(methodName));
	}
	Operator(String op, int codePoint, Position position, Precedence p, String methodName) {
		this(op, codePoint, ParenType.NONE, position, p, Associativity.LEFT, Option.some(methodName));
	}
	
	Operator(String op, Position position, Precedence p) {
		this(op, CODEPOINT_NONE, position, p);
	}
	Operator(String op, int cp, Position position, Precedence p, Associativity associativity) {
		this(op, cp, PAREN_NONE, position, p, associativity, METHOD_NONE);
	}
	Operator(String op, Position position, Precedence p, Associativity associativity) {
		this(op, CODEPOINT_NONE, position, p, associativity);
	}
	Operator(String op, Position position, Precedence p, String methodName) {
		this(op, CODEPOINT_NONE, position, p, methodName);
	}
	Operator(String op, int codePoint, Position position, Precedence p) {
		this(op, codePoint, position, p, Associativity.LEFT);
	}
	Operator(String op, int codePoint, Precedence p, Associativity associativity) {
		this(op, codePoint, PAREN_NONE, Position.INFIX, p, associativity, METHOD_NONE);
	}
	Operator(ParenType parenType, Position position) {
		this(String.valueOf(parenType.getStartChar()), CODEPOINT_NONE, Option.some(parenType), position, Precedence.SUFFIX, Associativity.LEFT, METHOD_NONE);
	}

	public static Option<Operator> fromOp(String op, Position pos) {
		for(Operator operator : values()) {
			// Wrong position
			if(operator.getPosition() != pos)
				continue;
			if(op.equals(operator.getOp()) || (operator.codePoint > 0 && op.length()==1 && op.codePointAt(0) == operator.codePoint)) {
				return Option.some(operator);
			}
		}
		return Option.none();
	}

	public static Option<Operator> fromParenType(ParenType pt, Position pos) {
		for(Operator operator : values()) {
			if(pt.equals(operator.parenType) && pos == operator.position) {
				return Option.some(operator);
			}
		}
		return Option.none();
	}
	public int getCodePoint() {
		return codePoint;
	}
	
	/**
	 * Get the type of paren this operator is associated with.  Fails if this is not associated
	 * with a paren.
	 */
	public ParenType getParenType() {
		return parenType.some();
	}
	
	/**
	 * @return true if this operator is a parenthesis type operator
	 */
	public boolean isParen() {
		return parenType.isSome();
	}
	public Option<String> getMethodName() {
		return methodName;
	}

	public Associativity getAssociativity() {
		return associativity;
	}
	
	public boolean isLeftAssociative() {
		return associativity == Associativity.LEFT;
	}
	
	public boolean isRightAssociative() {
		return associativity == Associativity.RIGHT;
	}
	
	

	Operator(String op, Precedence precedence, Position position, String methodName) {
		this(op, CODEPOINT_NONE, position, precedence, methodName);
	}	

	public String getOp() {
		return op;
	}

	public Precedence getPrecedence() {
		return precedence;
	}
	public Position getPosition() {
		return position;
	}
	public boolean isSuffix() {
		return position == Position.SUFFIX;
	}
	public boolean isInfix() {
		return position == Position.INFIX;
	}
	public boolean isPrefix() {
		return position == Position.PREFIX;
	}

	public boolean hasMethodName() {
		return methodName.isSome();
	}
	
}