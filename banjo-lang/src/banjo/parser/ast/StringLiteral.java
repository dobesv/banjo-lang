package banjo.parser.ast;

import java.io.IOException;
import java.util.Collection;

import banjo.parser.BanjoParser;
import banjo.parser.BanjoParser.BanjoParseException;
import banjo.parser.util.FilePos;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;
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
	
	/**
	 * Check whether the input stream is positioned before a string literal and, if so, parse it and return a StringLiteral node.  If not,
	 * leave the input in the same position as given and return null.
	 * 
	 * The parser supports the following escape sequences (prefixed with a backslash ('\') character:
	 * 
	 * <table>
	 * <thead><tr><td>Escape</td><td>Unicode Ordinal</td><td>Comment</td></thead>
	 * <tbody>
	 * <tr><td>\</td><td>92</td><td>Literal Backslash</td></tr>
	 * <tr><td>a</td><td>7</td><td>Bell</td></tr>
	 * <tr><td>e</td><td>27</td><td>Escape</td></tr> 
	 * <tr><td>f</td><td>12</td><td>Form Feed</td></tr> 
	 * <tr><td>n</td><td>13</td><td>Carriage Return</td></tr>
	 * <tr><td>r</td><td>10</td><td>Line Feed</td></tr>
	 * <tr><td>t</td><td>9</td><td>Horizontal Tab</td></tr>
	 * <tr><td>ooo</td><td></td><td>One to three octal digits will be interpreted as an octal number in the range 0 to 0777 (decimal 1024)</td></tr>
	 * <tr><td>xXX{2}</td><td></td><td>Two hex digits give a ASCII character code in the range 0-127</td></tr>
	 * <tr><td>uXXXX</td><td></td><td>Four hex digits give a unicode character in the range 0-65535</td></tr>
	 * </tbody>
	 * </table>
	 *
	 * TODO: Error reporting ...
	 */

	public static StringLiteral parseStringLiteral(ParserReader in, Collection<BanjoParseException> errors) throws IOException {
		FilePos startPos = in.getFilePos();
		String ignored = BanjoParser.consumeWhitespace(in);
		
		int cp = in.read();
		if(cp != '"') {
			in.seek(startPos);
			return null;
		}
			
		FilePos tokenStartPos = in.getFilePos();
	    StringBuffer buf = new StringBuffer(in.remaining());
	    while((cp = in.read()) != -1) {
	    	if(cp == '"')
	    		break; // End of string
            if(cp != '\\') {
                buf.appendCodePoint(cp);
	            continue;
            }
            FilePos afterBackslash = in.getFilePos();
            cp = in.read();
            if(cp == -1) {
                errors.add(new BadStringEscapeSequence("Backslash at end of string.", in.getFileRange(tokenStartPos))); // Shouldn't normally be possible
                break;
            }
	        switch (cp) {
	        	case '\\': buf.append('\\'); break;
	            case 'a':  buf.append('\007'); break;
	            case 'e':  buf.append('\033'); break;
	            case 'r':  buf.append('\r'); break;
	            case 'n':  buf.append('\n'); break;
	            case 'f':  buf.append('\f'); break;
	            case 't':  buf.append('\t'); break;
	            case '\'': case '"': buf.append((char)cp); break;
	            case '0': 
	            case '1':
	            case '2':
	            case '3':
	            case '4':
	            case '5':
	            case '6':
	            case '7': {
	            	readOctalEscape(in, cp, buf);
	                break;
	            }

	            case 'x':  {
	            	readHexEscape(in, errors, buf, 2);
	                break;
	            }

	            case 'u': {
	            	readHexEscape(in, errors, buf, 4);
	                break;
	            }

	            default:  {
                    errors.add(new BadStringEscapeSequence("Unknown escape sequence "+new String(Character.toChars(cp)), in.getFileRange(afterBackslash)));
                    break;
	            }
	        }
	    }
	    if(cp == -1) errors.add(new BanjoParser.PrematureEndOfFile("End of file in string literal", in.getFileRange(tokenStartPos)));
		return new StringLiteral(in.readTokenFrom(tokenStartPos, ignored), buf.toString());
	}

	private static void readOctalEscape(ParserReader in, int cp, StringBuffer buf) throws IOException {
		int result = Character.digit(cp, 8);
		FilePos afterDigits = in.getFilePos();
		for(int digits = 1; digits < 3; digits++) {
			int digitValue = Character.digit(in.read(), 8);
			if(digitValue == -1) {
				in.seek(afterDigits);
				return;
			} else {
				result = (result << 3) + digitValue;
				afterDigits = in.getFilePos();
			}
		}
		buf.appendCodePoint(result);
	}

	private static void readHexEscape(ParserReader in, Collection<BanjoParseException> errors, StringBuffer buf, final int digitCount) throws IOException {
		int result = 0;
		FilePos afterDigits = in.getFilePos();
		for(int digits = 0; digits < digitCount; digits++) {
			int digitValue = Character.digit(in.read(), 16);
			if(digitValue == -1) {
		    	errors.add(new BadStringEscapeSequence("Invalid hex digit in \\x escape", in.getFileRange(afterDigits)));
		    	in.seek(afterDigits);
		    	return;
			} else {
				result = (result << 4) | digitValue;
				afterDigits = in.getFilePos();
			}
		}
		buf.appendCodePoint(result);
	}

	public String getString() {
		return string;
	}
}
