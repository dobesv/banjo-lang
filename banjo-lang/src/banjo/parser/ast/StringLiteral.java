package banjo.parser.ast;


import banjo.parser.BanjoParser.BanjoParseException;
import banjo.parser.util.FileRange;
import banjo.parser.util.Token;

public class StringLiteral extends Expr {
	final Token token;
	private final String string;
	
	public StringLiteral(Token token, String string) {
		super();
		this.token = token;
		this.string = string;
	}
	
	public static class BadStringEscapeSequence extends BanjoParseException {
		private static final long serialVersionUID = 1L;

		public BadStringEscapeSequence(String message, FileRange range) {
			super(message, range);
		}
	}
	
	public String getString() {
		return string;
	}
}
