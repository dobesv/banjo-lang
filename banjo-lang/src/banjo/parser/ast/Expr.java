package banjo.parser.ast;


import banjo.parser.ast.Expr.Precedence;
import banjo.parser.util.FilePos;

public abstract class Expr {

	public static enum Precedence {
		// Listed here from HIGHEST to LOWEST
		ATOM, // Identifier, string/number/character/object literal/constructor 
		SUFFIX, // array index, member access, function/method call
		UNARY_PREFIX, // +, -, ~
		MULDIV, // *, /, %
		ADDSUB, // binary +, -
		BITSHIFT, // <<, >>, >>>
		ORDERING, // <, <=, >, >=
		EQUALITY, // ==, !=
		BITWISE_AND, // &
		BITWISE_XOR, // ^
		BITWISE_OR, // |
		LOGICAL_AND, // &&
		LOGICAL_OR, // ||
		TERNARY, // ? :
		ASSIGNMENT; // let, =, +=, *=, ...
		
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

}
