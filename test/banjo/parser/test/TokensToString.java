package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.token.Comment;
import banjo.dom.token.Ellipsis;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.dom.token.TokenVisitor;
import banjo.dom.token.UnitRef;
import banjo.dom.token.Whitespace;
import banjo.parser.BanjoScanner;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;

public class TokensToString implements TokenVisitor<String> {

	FileRange lastTokRange;
	final int rangeStart;
	final int rangeEnd;
	protected boolean done = false;

	public TokensToString(int rangeStart, int rangeEnd) {
		this.rangeStart = rangeStart;
		this.rangeEnd = rangeEnd;
	}

	@Override
	public String visitWhitespace(@NonNull FileRange range, @NonNull Whitespace tok) {
		return token("ws", range);
	}

	protected String token(String t, FileRange range) {
		assertEquals("Characters skipped?", this.lastTokRange != null ? this.lastTokRange.getEndOffset() : this.rangeStart, range.getStartOffset());
		this.lastTokRange = range;
		return t;
	}

	@Override
	public String visitComment(@NonNull FileRange range, @NonNull Comment tok) {
		return token("com", range);
	}

	@Override
	public String visitEof(@NonNull FileRange entireFileRange) {
		this.done = true;
		assertEquals(0, entireFileRange.getStartOffset());
		assertEquals(1, entireFileRange.getStartLine());
		assertEquals(1, entireFileRange.getStartColumn());
		final int fileLength = entireFileRange.getEndOffset();
		assertEquals("Wrong end of range?", this.rangeEnd, fileLength);
		return "eof";
	}

	@Override
	public String visitOperator(@NonNull FileRange range, @NonNull OperatorRef tok) {
		return token("op", range);
	}

	@Override
	public String visitStringLiteral(@NonNull FileRange range, @NonNull StringLiteral tok) {
		return token("str", range);
	}

	@Override
	public String visitNumberLiteral(@NonNull FileRange range, @NonNull NumberLiteral tok) {
		return token("num", range);
	}

	@Override
	public String visitIdentifier(@NonNull FileRange range, @NonNull Identifier tok) {
		return token("id", range);
	}

	@Override
	@Nullable
	public String visitEllipsis(@NonNull FileRange range, @NonNull Ellipsis ellipsis) {
		return token("...", range);
	}

	@Override
	@Nullable
	public String visitUnit(@NonNull FileRange range, @NonNull UnitRef unit) {
		return token("unit", range);
	}

	public static void testScanner(@NonNull String src, final int rangeStart, final int rangeEnd, String ... expectedTokens) {
		testScanner(src, rangeStart, rangeEnd, expectedTokens, new TokensToString(rangeStart, rangeEnd));
	}

	public static void testScanner(String src, final int rangeStart,
			final int rangeEnd, String[] expectedTokens,
			TokensToString testVisitor) throws Error {
		final ParserReader in = ParserReader.fromSubstring("<test>", src, rangeStart, rangeEnd);
		final BanjoScanner scanner = new BanjoScanner();

		final ArrayList<String> foundTokens = new ArrayList<String>(expectedTokens.length);
		while(!testVisitor.done) {
			try {
				foundTokens.add(scanner.next(in, testVisitor));
			} catch (final IOException e) {
				throw new Error(e);
			}
		}
		assertEquals(Arrays.asList(expectedTokens).toString(), foundTokens.toString());
	}

}