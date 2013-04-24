package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Comment;
import banjo.dom.Ellipsis;
import banjo.dom.HasFileRange;
import banjo.dom.Identifier;
import banjo.dom.NumberLiteral;
import banjo.dom.OperatorRef;
import banjo.dom.StringLiteral;
import banjo.dom.TokenVisitor;
import banjo.dom.UnitRef;
import banjo.dom.Whitespace;
import banjo.parser.BanjoScanner;
import banjo.parser.util.FileRange;
import banjo.parser.util.ParserReader;

public class TokensToString implements TokenVisitor<String> {

	HasFileRange lastTok;
	final int rangeStart;
	final int rangeEnd;
	protected boolean done = false;
	
	public TokensToString(int rangeStart, int rangeEnd) {
		this.rangeStart = rangeStart;
		this.rangeEnd = rangeEnd;
	}

	@Override
	public String visitWhitespace(@NonNull Whitespace tok) {
		return token("ws", tok);
	}

	protected String token(String t, HasFileRange tok) {
		assertEquals("Characters skipped?", lastTok != null ? lastTok.getFileRange().getEnd().getOffset() : rangeStart, tok.getFileRange().getStart().getOffset());
		lastTok = tok;
		return t;
	}

	@Override
	public String visitComment(@NonNull Comment tok) {
		return token("com", tok);
	}

	@Override
	public String visitEof(@NonNull FileRange entireFileRange) {
		done = true;
		assertEquals(0, entireFileRange.getStartOffset());
		assertEquals(1, entireFileRange.getStartLine());
		assertEquals(1, entireFileRange.getStartColumn());
		int fileLength = entireFileRange.getEndOffset();
		assertEquals("Wrong end of range?", this.rangeEnd, fileLength);
		return "eof";
	}

	@Override
	public String visitOperator(@NonNull OperatorRef tok) {
		return token("op", tok);
	}

	@Override
	public String visitStringLiteral(@NonNull StringLiteral tok) {
		return token("str", tok);
	}

	@Override
	public String visitNumberLiteral(@NonNull NumberLiteral tok) {
		return token("num", tok);
	}

	@Override
	public String visitIdentifier(@NonNull Identifier tok) {
		return token("id", tok);
	}
	
	@Override
	@Nullable
	public String visitEllipsis(@NonNull Ellipsis ellipsis) {
		return token("...", ellipsis);
	}
	
	@Override
	@Nullable
	public String visitUnit(@NonNull UnitRef unit) {
		return token("unit", unit);
	}

	public static void testScanner(@NonNull String src, final int rangeStart, final int rangeEnd, String ... expectedTokens) {
		testScanner(src, rangeStart, rangeEnd, expectedTokens, new TokensToString(rangeStart, rangeEnd));
	}

	public static void testScanner(String src, final int rangeStart,
			final int rangeEnd, String[] expectedTokens,
			TokensToString testVisitor) throws Error {
		ParserReader in = ParserReader.fromSubstring("<test>", src, rangeStart, rangeEnd);
		BanjoScanner scanner = new BanjoScanner();
		
		ArrayList<String> foundTokens = new ArrayList<String>(expectedTokens.length);
		while(!testVisitor.done) {
			try {
				foundTokens.add(scanner.next(in, testVisitor));
			} catch (IOException e) {
				throw new Error(e);
			}
		}
		assertEquals(Arrays.asList(expectedTokens).toString(), foundTokens.toString());
	}

}