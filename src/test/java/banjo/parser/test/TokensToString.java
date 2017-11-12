package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.io.UncheckedIOException;

import banjo.expr.token.TokenScanner;
import banjo.expr.token.TokenVisitor;
import banjo.expr.util.FileRange;
import banjo.expr.util.ParserReader;
import banjo.expr.util.TokenCollector;
import fj.data.List;

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
	public String whitespace(FileRange range, String tok) {
		return token("ws", range);
	}

	protected String token(String t, FileRange range) {
		final FileRange lastTokRange = this.lastTokRange;
		assertEquals("Characters skipped?", lastTokRange != null ? lastTokRange.getEndOffset() : this.rangeStart, range.getStartOffset());
		this.lastTokRange = range;
		return t;
	}

	@Override
	public String comment(FileRange range, String tok) {
		return token("com", range);
	}

	@Override
	public String eof(FileRange entireFileRange) {
		this.done = true;
		assertEquals(0, entireFileRange.getStartOffset());
		assertEquals(1, entireFileRange.getStartLine());
		assertEquals(1, entireFileRange.getStartColumn());
		final int fileLength = entireFileRange.getEndOffset();
		assertEquals("Wrong end of range?", this.rangeEnd, fileLength);
		return "eof";
	}

	@Override
	public String operator(FileRange range, int indentColumn, String tok) {
		return token("op", range);
	}

	@Override
    public String stringLiteral(FileRange range, int indentColumn, String tok) {
		return token("str", range);
	}

    @Override
    public String numberLiteral(FileRange range, int indentColumn, Number value, String source) {
		return token("num", range);
	}

    @Override
	public String identifier(FileRange range, int indentColumn, String tok) {
		return token("id", range);
	}

	public static void testScanner(String src, final int rangeStart, final int rangeEnd, String ... expectedTokens) {
		testScanner(src, rangeStart, rangeEnd, expectedTokens, new TokensToString(rangeStart, rangeEnd));
	}

	public static void testScanner(String src, final int rangeStart,
			final int rangeEnd, String[] expectedTokens,
			final TokensToString testVisitor) throws Error {
		final ParserReader in = ParserReader.fromSubstring("test", src, rangeStart, rangeEnd);
		final TokenScanner scanner = new TokenScanner();
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
	public String badToken(FileRange fileRange, String badToken, String message) {
		return token("bad", fileRange);
	}


}