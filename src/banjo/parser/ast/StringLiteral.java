package banjo.parser.ast;


import banjo.parser.errors.BanjoParseException;
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
		sb.append(toSource(string));
	}

	public static String toSource(String text) {
		return toSource(text, new StringBuffer(text.length()+10)).toString();
	}
	public static StringBuffer toSource(String text, StringBuffer sb) {
		sb.append('"');
		for(int i=0; i < text.length(); i++) {
			int cp = text.codePointAt(i);
			if(cp > Character.MAX_VALUE) i++; // Pair
			switch(cp) {
			case '\n': sb.append("\n"); break;
			case '\r': sb.append("\r"); break;
			case '\t': sb.append("\t"); break;
			case '\f': sb.append("\f"); break;
			case '"': sb.append("\\\""); break;
			default:
				sb.appendCodePoint(cp);
				break;
			}
		}
		sb.append('"');
		return sb;
	}
}
