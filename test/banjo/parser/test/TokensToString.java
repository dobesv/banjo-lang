package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.token.TokenVisitor;
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
	public String whitespace(@NonNull FileRange range, @NonNull String tok) {
		return token("ws", range);
	}

	protected String token(String t, FileRange range) {
		assertEquals("Characters skipped?", this.lastTokRange != null ? this.lastTokRange.getEndOffset() : this.rangeStart, range.getStartOffset());
		this.lastTokRange = range;
		return t;
	}

	@Override
	public String comment(@NonNull FileRange range, @NonNull String tok) {
		return token("com", range);
	}

	@Override
	public String eof(@NonNull FileRange entireFileRange) {
		this.done = true;
		assertEquals(0, entireFileRange.getStartOffset());
		assertEquals(1, entireFileRange.getStartLine());
		assertEquals(1, entireFileRange.getStartColumn());
		final int fileLength = entireFileRange.getEndOffset();
		assertEquals("Wrong end of range?", this.rangeEnd, fileLength);
		return "eof";
	}

	@Override
	public String operator(@NonNull FileRange range, @NonNull String tok) {
		return token("op", range);
	}

	@Override
	public String stringLiteral(@NonNull FileRange range, @NonNull String tok) {
		return token("str", range);
	}

	@Override
	public String numberLiteral(@NonNull FileRange range, @NonNull Number value) {
		return token("num", range);
	}

	@Override
	public String identifier(@NonNull FileRange range, @NonNull String tok) {
		return token("id", range);
	}

	public static void testScanner(@NonNull String src, final int rangeStart, final int rangeEnd, String ... expectedTokens) {
		testScanner(src, rangeStart, rangeEnd, expectedTokens, new TokensToString(rangeStart, rangeEnd));
	}

	public static void testScanner(@NonNull String src, final int rangeStart,
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

	@Override
	@Nullable
	public String badToken(@NonNull FileRange fileRange, @NonNull String badToken, @NonNull String message) {
		return token("bad", fileRange);
	}

}