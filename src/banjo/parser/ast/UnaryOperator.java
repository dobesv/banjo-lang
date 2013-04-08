package banjo.parser.ast;

public enum UnaryOperator implements Operator {
	PLUS("+", Precedence.UNARY_PREFIX, "plus"),
	NEGATE("-", Precedence.UNARY_PREFIX, "negate"),
	COMPLEMENT("~", Precedence.UNARY_PREFIX, "complement"),
	NOT("!", Precedence.UNARY_PREFIX, "false"),
	MIRROR(":", Precedence.UNARY_PREFIX),	
	LIST_ELEMENT("*", 0x2022, Precedence.BULLET),
	SET_ELEMENT("|", Precedence.BULLET),
	LAZY("->", 0x21a6, Precedence.FUNCTION),
	TABLE_HEADER("#::", Precedence.BULLET),
	TABLE_ROW("::", Precedence.BULLET),
	PARENS(ParenType.PARENS),
	LIST_LITERAL(ParenType.BRACKETS),
	RETURN("^", 0x2191, Precedence.ASSIGNMENT), // Basically a unary parenthesis
	OBJECT_OR_SET_LITERAL(ParenType.BRACES),
	CALL("<-", Precedence.UNARY_PREFIX), // Unary call with no parameters, possibly reading a lazy value
	OPTIONAL("?", Precedence.SUFFIX, Position.SUFFIX, "asOptionalContract"),
	EXISTS("??", Precedence.SUFFIX, Position.SUFFIX, "hasValue"),
	INVALID(null, Precedence.UNARY_PREFIX);
	
	public static enum Position { PREFIX, SUFFIX };
	
	static final int CODEPOINT_NA = -1;
	private final String op;
	private final Precedence precedence;
	private final int codePoint;
	private final ParenType parenType;
	private final String methodName;
	private final Position position;
	
	UnaryOperator(String op, int codePoint, Precedence precedence, ParenType parenType, String methodName, Position position) {
		this.op = op == null && codePoint != CODEPOINT_NA ? new String(Character.toChars(codePoint)) : op;
		this.codePoint = codePoint;
		this.precedence = precedence;
		this.parenType = parenType;
		this.methodName = methodName;
		this.position = position;
	}
	UnaryOperator(String op, Precedence precedence) {
		this(op, CODEPOINT_NA, precedence);
	}
	UnaryOperator(String op, Precedence precedence, Position position, String methodName) {
		this(op, CODEPOINT_NA, precedence, null, methodName, position);
	}	
	UnaryOperator(String op, Precedence precedence, String methodName) {
		this(op, CODEPOINT_NA, precedence, methodName);
	}
	
	UnaryOperator(String op, int cp, Precedence precedence) {
		this(op, cp, precedence, null);
	}
	
	UnaryOperator(ParenType pt) {
		this(null, CODEPOINT_NA, Precedence.ATOM, pt, null, Position.PREFIX);
	}
	UnaryOperator(String op, int codePoint, Precedence precedence, String methodName) {
		this(op, codePoint, precedence, null, methodName, Position.PREFIX);
	}
	public static UnaryOperator fromOp(String op, Position pos) {
		if(op == null)
			return null;
		for(UnaryOperator operator : values()) {
			// Wrong position
			if(operator.getPosition() != pos)
				continue;
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
	public String getMethodName() {
		return methodName;
	}
	public Position getPosition() {
		return position;
	}
	public boolean isSuffix() {
		return position == Position.SUFFIX;
	}
	
}
