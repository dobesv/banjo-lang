package banjo.dom.source;


public enum Precedence {
	// Listed here from HIGHEST to LOWEST
	ATOM, // Identifier, string/number/character/object literal/constructor, parenthesized expr
	SELECTOR,
	SUFFIX, // array index, member access, function/method call
	JUXTAPOSITION,
	UNARY_PREFIX, // +, -, ~, .
	MULDIV, // *, /, %
	ADDSUB, // +, -
	BITSHIFT, // <<, >>, >>>
	ORDERING, // <, <=, >, >=
	MEMBER_OF, // in
	EQUALITY, // ==, !=
	INTERSECT, // &
	XOR, // ^
	UNION, // |
	MATCH, // #, #?
	LAZY_AND, // &&
	LAZY_OR, // ||
	LET, // =>
	ALTERNATIVE, // |||
	TERNARY, // ? :
	COLON, // Key/value pair
	BULLET, // Bullets list
	FUNCTION, // ->
	EXTEND, // @
	ASSIGNMENT, // =, +=, *=, ...
	COND, // '=>' implication / cond
	COMMA, // ',' "sequence operator"
	SEMICOLON; // ';' "sequence operator"

	public boolean isHigherThan(Precedence other) {
		return ordinal() < other.ordinal();
	}
	public boolean isHigherOrEqual(Precedence other) {
		return ordinal() <= other.ordinal();
	}
	public boolean isLowerThan(Precedence other) {
		return ordinal() > other.ordinal();
	}
	public boolean isLowerOrEqual(Precedence other) {
		return ordinal() >= other.ordinal();
	}
	public Precedence nextHighest() {
		return values()[ordinal()-1];
	}
	public Precedence nextLowest() {
		return values()[ordinal()+1];
	}
	public static Precedence highest() {
		return values()[0];
	}
	public static Precedence lowest() {
		return values()[values().length-1];
	}

}