package banjo.parser.ast;


public enum BinaryOperator implements Operator {
	LOOKUP(ParenType.BRACKETS),
	CALL(ParenType.PARENS),
	MEMBER(".", Precedence.SUFFIX),
	POW("^", Precedence.MULDIV),
	MUL("*", 0x00D7, Precedence.MULDIV),
	DIV("/", 0x00F7, Precedence.MULDIV),
	ADD("+", Precedence.ADDSUB),
	SUB("-", Precedence.ADDSUB),
	GT(">", Precedence.ORDERING),
	GE(">=", 0x2265, Precedence.ORDERING),
	LE("<", Precedence.ORDERING),
	LT("<=", 0x2264, Precedence.ORDERING),
	EQ("==", Precedence.EQUALITY),
	NEQ("!=", 0x2260, Precedence.EQUALITY),
	INTERSECT("&", 0x2229, Precedence.INTERSECT),
	XOR("><", 0x22BB, Precedence.XOR),
	UNION("|", 0x222A, Precedence.UNION),
	LAZY_AND("&&", 0x2227, Precedence.LAZY_AND),
	LAZY_OR("||", 0x2228, Precedence.LAZY_OR),
	COMMA(",", Precedence.COMMA),
	FUNCTION("->", 0x21A6, Precedence.FUNCTION),
	ASSIGNMENT("=", Precedence.ASSIGNMENT),
	PAIR(":", Precedence.COLON),
	TABLE_PAIR("::", Precedence.ASSIGNMENT),
	COND("=>", 0x21D2, Precedence.COND),
	SEMICOLON(";", Precedence.SEMICOLON),
	NEWLINE("\n", Precedence.SEMICOLON),
	INVALID("~~~INVALID~~~", -1, Precedence.ATOM),
	MISSING("~~~MISSING~~~", -1, Precedence.ATOM); // Newline and indent
	
	private final String op;
	private final int codePoint; // -1 if no special unicode character
	private final Precedence precedence;
	private final ParenType parenType; // nullable
	
	BinaryOperator(String op, int codePoint, Precedence p, ParenType parenType) {
		this.op = op;
		this.codePoint = codePoint;
		this.precedence = p;
		this.parenType = parenType;
	}
	BinaryOperator(String op, Precedence p) {
		this(op, -1, p);
	}
	BinaryOperator(String op, int codePoint, Precedence p) {
		this(op, codePoint, p, null);
	}
	BinaryOperator(ParenType pt) {
		this(null, -1, Precedence.SUFFIX, pt);
	}

	public String getOp() {
		return op;
	}

	public Precedence getPrecedence() {
		return precedence;
	}

	public static BinaryOperator fromOp(String op) {
		if(op == null)
			return null;
		for(BinaryOperator operator : values()) {
			if(op.equals(operator.op) || (operator.codePoint > 0 && op.length() == 1 && op.codePointAt(0) == operator.codePoint)) {
				return operator;
			}
		}
		return null;
	}
	public static BinaryOperator fromParenType(ParenType pt) {
		if(pt == null)
			return null;
		for(BinaryOperator operator : values()) {
			if(pt.equals(operator.parenType)) {
				return operator;
			}
		}
		return null;
	}
	public int getCodePoint() {
		return codePoint;
	}
	public ParenType getParenType() {
		return parenType;
	}
	public boolean isParen() {
		return parenType != null;
	}
}