package banjo.parser.ast;


import banjo.parser.BanjoParser.BanjoParseException;
import banjo.parser.util.FileRange;
import banjo.parser.util.Token;

public class StringLiteral extends Expr {
	final Token token;
	private final String string;
	
	public StringLiteral(Token token, String string) {
		super(token.getFileRange());
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
	
	@Override
	public String toString() {
		return token.toString();
	}
	
	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		// TODO String escaping ...
		sb.append(token.toString());
	}
}
