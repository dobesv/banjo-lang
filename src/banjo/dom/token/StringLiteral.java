package banjo.dom.token;


import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.CoreExprVisitor;
import banjo.dom.source.Atom;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprVisitor;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.FileRange;

public class StringLiteral extends AbstractAtom implements Atom, Key {
	private final String string;

	public StringLiteral(int sourceLength, String string) {
		super(sourceLength);
		this.string = string;
	}

	public static class BadStringEscapeSequence extends BanjoParseException {
		private static final long serialVersionUID = 1L;

		public BadStringEscapeSequence(String message, FileRange range) {
			super(message, range);
		}
	}

	public String getString() {
		return this.string;
	}

	@Override
	public String getKeyString() {
		return this.string;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append(toSource(this.string));
	}

	public static String toSource(String text) {
		return nonNull(toSource(text, new StringBuffer(text.length()+10)).toString());
	}

	public static StringBuffer toSource(String text, StringBuffer sb) {
		sb.append('"');
		for(int i=0; i < text.length(); i++) {
			final int cp = text.codePointAt(i);
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

	@Override
	public @Nullable <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.visitStringLiteral(this);
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitStringLiteral(this);
	}

}
