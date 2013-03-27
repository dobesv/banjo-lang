package banjo.parser.ast;

public enum UnaryOperator implements Operator {
	PLUS("+", Precedence.UNARY_PREFIX),
	NEGATE("-", Precedence.UNARY_PREFIX),
	COMPLEMENT("~", Precedence.UNARY_PREFIX),
	KEYWORD(":", Precedence.UNARY_PREFIX),	
	LIST_ELEMENT("*", 0x2022, Precedence.BULLET),
	SET_ELEMENT("|", Precedence.BULLET),
	LAZY("->", 0x21a6, Precedence.FUNCTION),
	TABLE_HEADER("#::", Precedence.BULLET),
	TABLE_ROW("::", Precedence.BULLET),
	PARENS(ParenType.PARENS),
	LIST_LITERAL(ParenType.BRACKETS),
	RETURN("^", 0x2191, Precedence.ASSIGNMENT), // Return operator is used as the value of an expression list
	OBJECT_OR_SET_LITERAL(ParenType.BRACES),
	CALL("<-", Precedence.UNARY_PREFIX), // Unary call with no parameters, possibly reading a lazy value
	INVALID(null, Precedence.UNARY_PREFIX);
	
	private final String op;
	private final Precedence precedence;
	private final int codePoint;
	private final ParenType parenType;
	
	UnaryOperator(String op, int codePoint, Precedence precedence, ParenType parenType) {
		this.op = op;
		this.codePoint = codePoint;
		this.precedence = precedence;
		this.parenType = parenType;
	}
	UnaryOperator(String op, Precedence precedence) {
		this(op, -1, precedence);
	}
	
	UnaryOperator(String op, int cp, Precedence precedence) {
		this(op, cp, precedence, null);
	}
	
	UnaryOperator(ParenType pt) {
		this(null, -1, Precedence.ATOM, pt);
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
	public static UnaryOperator fromParenType(ParenType pt) {
		if(pt == null)
			return null;
		for(UnaryOperator operator : values()) {
			if(pt.equals(operator.parenType)) {
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
	public ParenType getParenType() {
		return parenType;
	}
	public boolean isParen() {
		return parenType != null;
	}
	
}
