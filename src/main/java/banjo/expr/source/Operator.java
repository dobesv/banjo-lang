package banjo.expr.source;

import static java.util.Objects.requireNonNull;

import java.util.Objects;

import banjo.expr.ParenType;
import banjo.expr.token.Identifier;
import fj.Ord;

public enum Operator {
	// Unary operators
	PLUS("+", OperatorType.METHOD, null, Position.PREFIX, Associativity.RIGHT, Precedence.UNARY_PREFIX, Precedence.UNARY_PREFIX, "+_"),
	NEGATE("-", OperatorType.METHOD, null, Position.PREFIX, Associativity.RIGHT, Precedence.UNARY_PREFIX, Precedence.UNARY_PREFIX, "-_"),
	COMPLEMENT("~", OperatorType.METHOD, Position.PREFIX, Precedence.UNARY_PREFIX),
	NOT("¬ !", OperatorType.METHOD, Position.PREFIX, Precedence.UNARY_PREFIX),
	LIST_ELEMENT("• *",OperatorType.BUILTIN, Position.PREFIX, Precedence.BULLET),
	NULLARY_FUNCTION_LITERAL("↦ ->", OperatorType.BUILTIN, Position.PREFIX, Precedence.FUNCTION),
	TABLE_HEADER("::", OperatorType.BUILTIN, Position.PREFIX, Precedence.BULLET),
	TABLE_ROW(":::", OperatorType.BUILTIN, Position.PREFIX, Precedence.BULLET),
	PARENS(ParenType.PARENS, OperatorType.BUILTIN, Position.PREFIX),
	LIST_LITERAL(ParenType.BRACKETS, OperatorType.BUILTIN, Position.PREFIX),
	ABSVALUE(ParenType.ABSVALUE, OperatorType.METHOD, Position.PREFIX),
	OBJECT_LITERAL(ParenType.BRACES, OperatorType.BUILTIN, Position.PREFIX),
	INSPECT("$", OperatorType.FUNCTION, Precedence.UNARY_PREFIX, Position.PREFIX),
	PROJECTION_FUNCTION(".", OperatorType.BUILTIN, Precedence.SELECTOR, Position.PREFIX),
	EXTENSION_FUNCTION("Φ @", OperatorType.FUNCTION, Precedence.SELECTOR, Position.PREFIX),
	BASE_FUNCTION("↑ ^", OperatorType.BUILTIN, Position.PREFIX, Precedence.SUFFIX),
	PASS_TO_LEFT_FUNCTION("◄ <|", OperatorType.FUNCTION, Precedence.UNARY_PREFIX, Position.PREFIX),
    QUICK_LAMBDA("?", OperatorType.BUILTIN, Position.PREFIX, Precedence.SELECTOR),

	// Binary operators
	CALL(ParenType.PARENS, OperatorType.BUILTIN, Position.INFIX),
	EXTENSION("Φ @", OperatorType.BUILTIN, Position.INFIX, Precedence.EXTEND),
	PROJECTION(".", OperatorType.BUILTIN, Position.INFIX, Precedence.SUFFIX),
	PROJECTION_OF_MEMBERS("*.", OperatorType.BUILTIN, Position.INFIX, Precedence.SUFFIX),
	BASE_SLOT(":", OperatorType.BUILTIN, Position.INFIX, Precedence.SUFFIX),
	POW("^", OperatorType.METHOD, Position.INFIX, Precedence.MULDIV),
	MUL("× *", OperatorType.METHOD, Position.INFIX, Precedence.MULDIV),
	DIV("÷ / ∕", OperatorType.METHOD, Position.INFIX, Precedence.MULDIV),
	ADD("+", OperatorType.METHOD, Position.INFIX, Precedence.ADDSUB),
	SUB("-", OperatorType.METHOD, Position.INFIX, Precedence.ADDSUB),
	LT("<", OperatorType.METHOD, Position.INFIX, Precedence.ORDERING),
	GT(">", OperatorType.METHOD_SWITCHED, Position.INFIX, Precedence.ORDERING, "<"),
	LE("≤ <=", OperatorType.BUILTIN, Position.INFIX, Precedence.ORDERING),
	GE("≥ >=", OperatorType.BUILTIN, Position.INFIX, Precedence.ORDERING, "≤"),
	EQ("==", OperatorType.METHOD, Position.INFIX, Precedence.EQUALITY),
	NEQ("≠ !=", OperatorType.BUILTIN, Position.INFIX, Precedence.EQUALITY),
	CMP("<=> ⋚ ⋛", OperatorType.METHOD, Position.INFIX, Precedence.EQUALITY),
	MEMBER_OF("∈ <++", OperatorType.METHOD_SWITCHED, null, Position.INFIX, Associativity.NA, Precedence.MEMBER_OF, Precedence.MEMBER_OF, "∈"),
	INTERSECT("∩ &", OperatorType.METHOD, Position.INFIX, Precedence.INTERSECT),
	XOR("⊻ ><", OperatorType.METHOD, Position.INFIX, Precedence.XOR),
	UNION("∪ |", OperatorType.METHOD, Position.INFIX, Precedence.UNION),
	LOGICAL_AND("∧ &&", OperatorType.METHOD, Position.INFIX, Precedence.LOGICAL_AND),
	LOGICAL_OR("∨ ||", OperatorType.METHOD, Position.INFIX, Precedence.LOGICAL_OR),
	FALLBACK("?:", OperatorType.METHOD, Position.INFIX, Precedence.LOGICAL_OR),

	PASS_TO("► |>", OperatorType.BUILTIN, Position.INFIX, Precedence.LOGICAL_OR),
	PASS_TO_LEFT("◄ <|", OperatorType.BUILTIN, Position.INFIX, Precedence.LOGICAL_OR),
	FUNCTION_COMPOSITION_LEFT("∘ <<", OperatorType.METHOD_SWITCHED, Position.INFIX, Precedence.FUNCTION_COMPOSITION),
	FUNCTION_COMPOSITION_RIGHT("； ; >>", OperatorType.METHOD, null, Position.INFIX, Associativity.LEFT, Precedence.LOGICAL_OR, Precedence.FUNCTION_COMPOSITION, "∘"),

	PRECONDITION("&&&", OperatorType.BUILTIN, Position.INFIX, Precedence.LOGICAL_AND),
	FUNCTION("↦ ->", OperatorType.BUILTIN, Position.INFIX, Precedence.SUFFIX, Precedence.FUNCTION, Associativity.RIGHT),
	ASSIGNMENT("=", OperatorType.BUILTIN, Position.INFIX, Precedence.ASSIGNMENT, Associativity.RIGHT),
	EXTEND_METHOD("Φ= @=", OperatorType.BUILTIN, Position.INFIX, Precedence.ASSIGNMENT, Associativity.NA),
	ADD_METHOD("+=", OperatorType.BUILTIN, Position.INFIX, Precedence.ASSIGNMENT, Associativity.NA),
	SUB_METHOD("-=", OperatorType.BUILTIN, Position.INFIX, Precedence.ASSIGNMENT, Associativity.NA),
	MUL_METHOD("×= *=", OperatorType.BUILTIN, Position.INFIX, Precedence.ASSIGNMENT, Associativity.NA),
	DIVID_METHOD("÷= /=", OperatorType.BUILTIN, Position.INFIX, Precedence.ASSIGNMENT, Associativity.NA),
	UNION_METHOD("∪= |=", OperatorType.BUILTIN, Position.INFIX, Precedence.ASSIGNMENT, Associativity.NA),
	AND_METHOD("∧= &&=", OperatorType.BUILTIN, Position.INFIX, Precedence.ASSIGNMENT, Associativity.NA),
	OR_METHOD("∨= ||=", OperatorType.BUILTIN, Position.INFIX, Precedence.ASSIGNMENT, Associativity.NA),

	// MONAD_EXTRACT("<-", OperatorType.BUILTIN, Position.INFIX, Precedence.ASSIGNMENT, Associativity.RIGHT),

	LET("⇒ =>", OperatorType.BUILTIN, Position.INFIX, Precedence.LET, Precedence.COND, Associativity.NA),
	COMMA(",", OperatorType.BUILTIN, Position.INFIX, Precedence.COMMA, Associativity.RIGHT),
	NEWLINE("(nl) \n", OperatorType.BUILTIN, Position.INFIX, Precedence.SEMICOLON, Associativity.RIGHT),

	JUXTAPOSITION("~~~JUXTAPOSITION~~~", OperatorType.BUILTIN, Position.INFIX, Precedence.JUXTAPOSITION, Precedence.SUFFIX, Associativity.LEFT),

	// Special case operators
	INVALID("~~~INVALID~~~", OperatorType.BUILTIN, Position.NA, Precedence.ATOM),
	MISSING("~~~MISSING~~~", OperatorType.BUILTIN, Position.NA, Precedence.ATOM); // Newline and indent

	public static final Ord<Operator> ORD = Ord.<Operator>comparableOrd();

	public static enum Associativity { LEFT, RIGHT, NA; }
	public static enum Position { PREFIX, INFIX, SUFFIX, NA }
	public final Precedence leftPrecedence; // For binary operators only
	public final Precedence precedence;
	public final ParenType parenType; // nullable
	public final Associativity associativity;
	public final Position position;
	public final String methodName;
	public final Identifier methodNameKey;
	public final OperatorType operatorType;
	public final String[] ops;

	Operator(String ops, OperatorType operatorType, ParenType parenType, Position position, Associativity associativity, Precedence leftPrecedence, Precedence precedence, String methodName) {
		this.ops = ops.split(" ");
		this.operatorType = requireNonNull(operatorType);
		this.leftPrecedence = requireNonNull(leftPrecedence);
		this.precedence = requireNonNull(precedence);
		this.parenType = parenType;
		this.associativity = requireNonNull(associativity);
		this.position = requireNonNull(position);
		this.methodName = parenType == ParenType.PARENS || methodName == null? null : methodName.replaceFirst("\\?$", "");
		this.methodNameKey = this.methodName == null ? null : new Identifier(this.methodName);
	}

	Operator(String ops, OperatorType operatorType, ParenType parenType, Position position, Associativity associativity, Precedence leftPrecedence, Precedence precedence) {
		this(ops, operatorType, parenType, position, associativity, leftPrecedence, precedence,
				defaultOpMethodName(ops, position, associativity, operatorType));
	}

	static String defaultOpMethodName(String ops, Position position,
            Associativity associativity, OperatorType type) {
		if(type == OperatorType.BUILTIN)
			return null;
		String op = ops.split(" ", 2)[0];
//		switch(position) {
//		case PREFIX:
//			return op+" _";
//		case SUFFIX:
//			return "_ "+op;
//		case INFIX:
//			return "_ "+op+" _";
//		default:
//			return null;
//		}
		return op;
    }

	public static Associativity defaultAssociativity(Position position) {
		switch(position) {
		case PREFIX: return Associativity.RIGHT;
		default:
		case INFIX:
		case SUFFIX: return Associativity.LEFT;
		}
	}

	Operator(String ops, OperatorType operatorType, Position position, Precedence p) {
		this(ops, operatorType, position, p, defaultAssociativity(position));
	}
	Operator(String ops, OperatorType operatorType, Position position, Precedence p, String methodName) {
		this(ops, operatorType, null, position, defaultAssociativity(position), p, p, methodName);
	}
	Operator(String ops, OperatorType operatorType, Position position, Precedence precedence, Associativity associativity) {
		this(ops, operatorType, position, precedence, precedence, associativity);
	}
	Operator(String ops, OperatorType operatorType, Position position, Precedence leftPrecedence, Precedence rightPrecedence, Associativity associativity) {
		this(ops, operatorType, null, position, associativity, leftPrecedence, rightPrecedence);
	}
	Operator(String ops, OperatorType operatorType, Precedence p, Associativity associativity) {
		this(ops, operatorType, null, Position.INFIX, associativity, p, p);
	}
	Operator(ParenType parenType, OperatorType operatorType, Position position) {
		this(parenType.getEmptyPairString(), operatorType, parenType, position, defaultAssociativity(position), Precedence.SUFFIX, Precedence.SUFFIX);
	}

	public static Operator fromOp(String op, Position pos) {
		for(final Operator operator : values()) {
			// Wrong position
			if(operator.getPosition() != pos)
				continue;
			if(operator.isParen() && op.length() == 1) {
				if(operator.getParenType().getStartChar() == op.charAt(0))
					return operator;
			} else for(String tryOp : operator.ops) if(op.equals(tryOp)) {
				return operator;
			}
		}
		return null;
	}

	public static Operator fromMethodName(String methodName, boolean infix) {
		for(final Operator operator : values()) {
			if(infix == operator.isInfix() && operator.methodName != null && methodName.equals(operator.methodName)) {
				return operator;
			}
		}
		return null;
	}

	public static Operator fromMethodName(Identifier methodName, boolean infix) {
		for(final Operator operator : values()) {
			if(infix == operator.isInfix() && operator.methodNameKey != null && methodName.eql(operator.methodNameKey)) {
				return operator;
			}
		}
		return null;
	}

	public static Operator fromParenType(ParenType pt, Position pos) {
		for(final Operator operator : values()) {
			if(pt.equals(operator.parenType) && pos == operator.position) {
				return operator;
			}
		}
		return null;
	}


	/**
	 * Get the type of paren this operator is associated with.  Fails if this is not associated
	 * with a paren.
	 */
	public ParenType getParenType() {
		return Objects.requireNonNull(this.parenType);
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
		this(op, operatorType, position, precedence);
	}

	public String getOp() {
		return this.ops[0];
	}

	public Precedence getLeftPrecedence() {
		return this.leftPrecedence;
	}
	public Precedence getRightPrecedence() {
		return this.precedence;
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
		sb.append(ops[0]);
	}

	public String getMethodName() {
		return this.methodName;
	}

	public Identifier getMethodIdentifier() {
		return methodNameKey;
	}

	public OperatorType getOperatorType() {
		return this.operatorType;
	}

	public boolean isSelfOnRightMethodOperator() {
		return isPrefix() || (isInfix() && getOperatorType() == OperatorType.METHOD_SWITCHED);
	}
}