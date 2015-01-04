package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.io.UncheckedIOException;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.token.TokenVisitor;
import banjo.parser.SourceCodeScanner;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;
import banjo.parser.util.TokenCollector;
import fj.data.List;

@NonNullByDefault
public class TokensToString implements TokenVisitor<@NonNull String> {

	@Nullable
	FileRange lastTokRange;
	final int rangeStart;
	final int rangeEnd;
	protected boolean done = false;

	public TokensToString(int rangeStart, int rangeEnd) {
		this.rangeStart = rangeStart;
		this.rangeEnd = rangeEnd;
	}

	@Override
	public @NonNull String whitespace(@NonNull FileRange range, @NonNull String tok) {
		return token("ws", range);
	}

	protected String token(String t, FileRange range) {
		final @Nullable FileRange lastTokRange = this.lastTokRange;
		assertEquals("Characters skipped?", lastTokRange != null ? lastTokRange.getEndOffset() : this.rangeStart, range.getStartOffset());
		this.lastTokRange = range;
		return t;
	}

	@Override
	public @NonNull String comment(@NonNull FileRange range, @NonNull String tok) {
		return token("com", range);
	}

	@Override
	public @NonNull String eof(@NonNull FileRange entireFileRange) {
		this.done = true;
		assertEquals(0, entireFileRange.getStartOffset());
		assertEquals(1, entireFileRange.getStartLine());
		assertEquals(1, entireFileRange.getStartColumn());
		final int fileLength = entireFileRange.getEndOffset();
		assertEquals("Wrong end of range?", this.rangeEnd, fileLength);
		return "eof";
	}

	@Override
	public @NonNull String operator(@NonNull FileRange range, @NonNull String tok) {
		return token("op", range);
	}

	@Override
	public @NonNull String stringLiteral(@NonNull FileRange range, @NonNull String tok) {
		return token("str", range);
	}

	@Override
	public @NonNull String numberLiteral(@NonNull FileRange range, @NonNull Number value, @NonNull String suffix) {
		return token("num", range);
	}

	@Override
	public @NonNull String identifier(@NonNull FileRange range, @NonNull String tok) {
		return token("id", range);
	}

	public static void testScanner(String src, final int rangeStart, final int rangeEnd, String ... expectedTokens) {
		testScanner(src, rangeStart, rangeEnd, expectedTokens, new TokensToString(rangeStart, rangeEnd));
	}

	public static void testScanner(String src, final int rangeStart,
			final int rangeEnd, String[] expectedTokens,
			final TokensToString testVisitor) throws Error {
		final ParserReader in = ParserReader.fromSubstring("<test>", src, rangeStart, rangeEnd);
		final SourceCodeScanner scanner = new SourceCodeScanner();
		List<String> foundTokens;
		try {
			final TokenCollector collector = new TokenCollector();
			foundTokens = scanner.scan(in, collector).getTokens().map(
					a -> testVisitor.done ? "": (String)a.acceptVisitor(testVisitor)
			);
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
		assertEquals(List.list(expectedTokens).toString(), foundTokens.toString());
	}

	@Override
	public @NonNull String badToken(@NonNull FileRange fileRange, @NonNull String badToken, @NonNull String message) {
		return token("bad", fileRange);
	}

}