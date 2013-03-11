package banjo.parser.ast;


public enum BinaryOperator {
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
	BITWISE_AND("&", Precedence.BITWISE_AND),
	BITWISE_XOR("^", Precedence.BITWISE_XOR),
	BITWISE_OR("|", Precedence.BITWISE_OR),
	LOGICAL_AND("&&", Precedence.LOGICAL_AND),
	LOGICAL_OR("||", Precedence.LOGICAL_OR),
	COMMA(",", Precedence.COMMA),
	FUNCTION("->", 0x21A6, Precedence.FUNCTION),
	ASSIGNMENT("=", Precedence.ASSIGNMENT),
	COLON(":", Precedence.BULLET),
	COND("=>", 0x21D2, Precedence.COND),
	SEMICOLON(";", Precedence.SEMICOLON);
	
	private final String op;
	private final int codePoint;
	private final Precedence precedence;

	BinaryOperator(String op, int codePoint, Precedence p) {
		this.op = op;
		this.codePoint = codePoint;
		this.precedence = p;
	}
	BinaryOperator(String op, Precedence p) {
		this(op, -1, p);
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
	public int getCodePoint() {
		return codePoint;
	}
}