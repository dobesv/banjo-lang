package banjo.parser.ast;

import java.io.IOException;
import java.text.ParseException;
import java.util.Collection;

import banjo.parser.BanjoParser.BanjoParseException;
import banjo.parser.ast.Expr.Precedence;
import banjo.parser.util.FilePos;
import banjo.parser.util.ParserReader;

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
	/**
	 * Attempt to parse an expression in the input stream.
	 * 
	 * @param in Input code to parse
	 * @param errors Recoverable errors - i.e. looks a bit off but we can carry on anyway
	 * @return An Expr subclass for the expression that was parsed.
	 * @throws IOException If the reader throws an IOException
	 * @throws ParseException If an unrecoverable parse error occurs
	 */
	public static Expr parseAnyExpr(ParserReader in, Precedence minimumPrecedence, Collection<BanjoParseException> errors) throws IOException, ParseException {
		// Atoms are always an option as they are the highest precedence
		IdRef idRef = IdRef.parseIdRef(in, errors);
		if(idRef != null) return idRef;
		StringLiteral strLit = StringLiteral.parseStringLiteral(in, errors);
		if(strLit != null) return strLit;
		NumberLiteral numLit = NumberLiteral.parseNumberLiteral(in, errors);
		if(numLit != null) return numLit;
		
		// Assignment / let
		if(minimumPrecedence.isLowerOrEqual(Precedence.ASSIGNMENT)) {
			Let node = Let.parseLet(in, errors);
			if(node != null) return node;
		}
		
		return null;
	}

}
