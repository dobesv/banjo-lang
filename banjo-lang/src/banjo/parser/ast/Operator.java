package banjo.parser.ast;


public enum Operator {
	MUL("*", Precedence.MULDIV),
	DIV("/", Precedence.MULDIV),
	ADD("+", Precedence.ADDSUB),
	SUB("-", Precedence.ADDSUB),
	GT(">", Precedence.ORDERING),
	GE(">=", Precedence.ORDERING),
	LE("<", Precedence.ORDERING),
	LT("<=", Precedence.ORDERING),
	EQ("==", Precedence.EQUALITY),
	NEQ("!=", Precedence.EQUALITY),
	BITWISE_AND("&", Precedence.BITWISE_AND),
	BITWISE_XOR("^", Precedence.BITWISE_XOR),
	BITWISE_OR("|", Precedence.BITWISE_OR),
	LOGICAL_AND("&&", Precedence.LOGICAL_AND),
	LOGICAL_OR("||", Precedence.LOGICAL_OR);
	
	private final String op;
	private final Precedence precedence;

	Operator(String op, Precedence p) {
		this.op = op;
		this.precedence = p;
	}

	public String getOp() {
		return op;
	}

	public Precedence getPrecedence() {
		return precedence;
	}

}