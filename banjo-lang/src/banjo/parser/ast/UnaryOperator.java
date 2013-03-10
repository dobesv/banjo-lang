package banjo.parser.ast;

public enum UnaryOperator {
	PLUS("+", Precedence.UNARY_PREFIX),
	NEGATE("-", Precedence.UNARY_PREFIX),
	COMPLEMENT("~", Precedence.UNARY_PREFIX),
	BULLET("*", 0x2022, Precedence.BULLET),
	LAZY("->", 0x21a6, Precedence.FUNCTION);
	
	private final String op;
	private final Precedence precedence;
	private final int codePoint;
	UnaryOperator(String op, int codePoint, Precedence precedence) {
		this.op = op;
		this.codePoint = codePoint;
		this.precedence = precedence;
	}
	UnaryOperator(String op, Precedence precedence) {
		this(op, -1, precedence);
	}
	
	public static UnaryOperator fromOp(String op) {
		if(op == null)
			return null;
		for(UnaryOperator operator : values()) {
			if(op.equals(operator.getOp()) || (operator.codePoint > 0 && op.length()==1 && op.codePointAt(0) == operator.codePoint)) {
				return operator;
			}
		}
		return null;
	}

	public String getOp() {
		return op;
	}

	public Precedence getPrecedence() {
		return precedence;
	}
	public int getCodePoint() {
		return codePoint;
	}
	
}
