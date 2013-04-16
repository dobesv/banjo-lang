package banjo.dom;


public enum Precedence {
	// Listed here from HIGHEST to LOWEST
	ATOM, // Identifier, string/number/character/object literal/constructor, parenthesized expr 
	SUFFIX, // array index, member access, function/method call
	UNARY_PREFIX, // +, -, ~
	MULDIV, // *, /, %
	ADDSUB, // +, -
	BITSHIFT, // <<, >>, >>>
	ORDERING, // <, <=, >, >=
	EQUALITY, // ==, !=
	INTERSECT, // &
	XOR, // ^
	UNION, // |
	LAZY_AND, // &&
	LAZY_OR, // ||
	TERNARY, // ? :
	COLON, // Key/value pair
	COMMA, // ',' "sequence operator"
	BULLET, // Bullets list
	FUNCTION, // ->
	ASSIGNMENT, // let, =, +=, *=, ...
	COND, // '=>' implication / cond
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