package banjo.parser;

import java.io.IOException;
import java.text.ParseException;
import java.util.Collection;
import java.util.regex.Pattern;

import banjo.parser.BanjoParser.BanjoParseException;
import banjo.parser.ast.*;
import banjo.parser.ast.Expr.Precedence;
import banjo.parser.util.*;

/**
 * Change input into an AST.
 */
public class BanjoParser {

	public static Expr parse(ParserReader in, Collection<BanjoParseException> errors) throws IOException, ParseException {
		FilePos startPos = in.getFilePos();
		try {
			return Expr.parseAnyExpr(in, Precedence.lowest(), errors);
		} catch(BanjoParseException pe) {
			errors.add(pe);
			return null;
		}
	}
	public static Pattern whitespace = Pattern.compile("([ \r\n]*|//[^\n]*|/\\*.*?\\*/)*", Pattern.DOTALL);
	public static Pattern idPattern = Pattern.compile("[\\p{Alpha}_-][\\p{Alnum}_-]*");
	public static Pattern opPattern = Pattern.compile("((=|\\+|-|/|\\*|&|^|>>|<<|\\^|\\$|\\#|\\@|!|\\|)=?|\\(|\\)|\\{|\\}|\\[|\\])");
	public static Pattern sepPattern = Pattern.compile("[;,]");
	/**
	 * Attempt to parse an ID token; returns null on failure.
	 * 
	 * The parse position will be just after the matched identifier on success, or the
	 * original starting position on failure.
	 * @param in
	 * @return
	 * @throws IOException 
	 */
	public static Token parseID(ParserReader in) throws IOException {
		FilePos startPos = in.getFilePos();
		String ignored = consumeWhitespace(in);
		Token result = in.checkNextToken(idPattern, ignored);
		if(result == null) in.seek(startPos);
		return result;
	}

	public static String consumeWhitespace(ParserReader in) throws IOException {
		return in.consume(whitespace);
	}

	/**
	 * Check whether the given operator follows.  If so, consume it and return true.  If not, return the stream in the
	 * same position and return false;
	 */
	public static boolean checkOperator(ParserReader in, String op) throws IOException {
		FilePos startPos = in.getFilePos();
		String ignored = consumeWhitespace(in);
		Token token = in.checkNextToken(opPattern, ignored);
		boolean result = token != null && token.getText().equals(op);
		if(!result) in.seek(startPos);
		return result;
	}

	/**
	 * Check whether the given separator follows.  If so, consume it and return true.  If not, but the next non-whitespace character
	 * is on a new line from where we started, consume the whitespace and return true.  If that fails, rewind and return false.
	 * 
	 * @param in Source input
	 * @param sep Seperator we are looking for; e.g. ";"
	 * @param indentColumn If a newline is used as a separator, the indentation must be at least this much or a parse exception will be thrown 
	 * @param errors 
	 * @return
	 * @throws IOException
	 * @throws IncorrectIndentation 
	 */
	public static boolean checkSeparatorOrNewline(ParserReader in, String sep, int indentColumn, Collection<BanjoParseException> errors) throws IOException {
		FilePos startPos = in.getFilePos();
		String ignored = consumeWhitespace(in);
		FilePos secondPos = in.getFilePos();
		Token token = in.checkNextToken(sepPattern, ignored);
	    boolean result = token != null && token.getText().equals(sep);
	    if(!result) {
	    	if(secondPos.line > startPos.line) {
	    		in.seek(secondPos);
	    		if(secondPos.col != indentColumn)
	    			errors.add(new IncorrectIndentation(in.getFileRange(in.getFilePos().lineStart()), indentColumn));
	    		return true;
	    	} else {
		    	in.seek(startPos);
		    	return false;
	    	}
	    }
	    return false;
	}
	
	public static class BanjoParseException extends java.text.ParseException {
		private static final long serialVersionUID = 1L;
		private final FileRange range;

		public BanjoParseException(String message, FileRange range) {
			super(message, range.getStartOffset());
			this.range = range;
		}

		public int getStartLine() { return range.getStart().line; }
		public int getStartColumn() { return range.getStart().col; }
		public int getEndLine() { return range.getEnd().line; }
		public int getEndColumn() { return range.getEnd().col; }
	}
	public static class ExpectedSemiColonOrNewline extends BanjoParseException {
		private static final long serialVersionUID = 1L;

		public ExpectedSemiColonOrNewline(FileRange range) {
			super("Expected semicolon or newline", range);
		}
	}
	public static class IncorrectIndentation extends BanjoParseException {
		private static final long serialVersionUID = 1L;

		public IncorrectIndentation(FileRange range, int indentColumn) {
			super("Expected indentation to column "+indentColumn+" but indentation was "+range.getEnd().col+" columns.", range);
		}
	}
	
	public static class PrematureEndOfFile extends BanjoParseException {
		private static final long serialVersionUID = 1L;
	
		public PrematureEndOfFile(String message, FileRange range) {
			super(message, range);
		}
	
	}

	public static class UnexpectedDecimalPoint extends BanjoParseException {
		private static final long serialVersionUID = 1L;
	
		public UnexpectedDecimalPoint(String message, FileRange range) {
			super(message, range);
		}
	
	}
	public static class UnexpectedExponent extends BanjoParseException {
		private static final long serialVersionUID = 1L;

		public UnexpectedExponent(String message, FileRange range) {
			super(message, range);
			// TODO Auto-generated constructor stub
		}
	
	}
	public static class UnexpectedSecondDecimalPoint extends UnexpectedDecimalPoint {
		private static final long serialVersionUID = 1L;

		public UnexpectedSecondDecimalPoint(String message, FileRange range) {
			super(message, range);
		}
	
	}

	/**
	 * Skip whitespace and attempt to parse the next token by matching the given regular
	 * expression.  If the regular expression doesn't match, rewaind to before any skipped
	 * whitespace and return null.  If it does match, consume the token and return it.
	 */
	public static Token checkPattern(ParserReader in, Pattern p) throws IOException {
		FilePos startPos = in.getFilePos();
		String ignored = consumeWhitespace(in);
		Token tok = in.checkNextToken(p, ignored);
		if(tok == null)
			in.seek(startPos);
		return tok;
	}
}
