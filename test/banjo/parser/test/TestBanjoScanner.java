package banjo.parser.test;

import static banjo.dom.test.ParseTestUtils.test;
import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Ignore;
import org.junit.Test;

import banjo.dom.Comment;
import banjo.dom.Ellipsis;
import banjo.dom.Expr;
import banjo.dom.HasFileRange;
import banjo.dom.Identifier;
import banjo.dom.NumberLiteral;
import banjo.dom.ObjectLiteral;
import banjo.dom.OperatorRef;
import banjo.dom.StringLiteral;
import banjo.dom.TokenVisitor;
import banjo.dom.UnitRef;
import banjo.dom.Whitespace;
import banjo.parser.BanjoParser;
import banjo.parser.BanjoScanner;
import banjo.parser.util.FilePos;
import banjo.parser.util.ParserReader;
import banjo.parser.util.TokenCollector;

public class TestBanjoScanner {
	@Test
	public void test1() {
		testTokenizer("// comment\n/* comment */\nfoo: bar", "{foo: bar}", ObjectLiteral.class,
				new String[] {
						"// comment\n",
						"/* comment */", " ",
						"foo", ":",	" ", "bar"
				}, new Class<?>[] {
						Comment.class, 
						Comment.class, Whitespace.class, 
						Identifier.class, OperatorRef.class, Whitespace.class, Identifier.class				
				});
	}

	private void testTokenizer(@NonNull String src, String normalizedSource,
			Class<? extends Expr> expectedClass,
			String[] expectedTokenNormalizedSource,
			Class<?>[] expectedTokenClasses) throws Error {
		BanjoScanner scanner = new BanjoScanner();
		ArrayList<HasFileRange> tokens = new ArrayList<>();
		scanner.scan(src, new TokenCollector(new BanjoParser(), tokens));
		final BanjoParser parser = new BanjoParser();
		test(src, 0, null, expectedClass, normalizedSource, parser);
		int expectedTokenCount = expectedTokenNormalizedSource.length;
		assertEquals(expectedTokenCount, tokens.size());
		for(int i=0; i < expectedTokenCount; i++) {
			HasFileRange token = tokens.get(i);
			assertEquals(expectedTokenClasses[i], token.getClass());
			assertEquals(expectedTokenNormalizedSource[i], token.toString());
		}
	}
	
	static class TestingSourceTokenVisitor implements TokenVisitor<String> {

		HasFileRange lastTok;
		final int rangeStart;
		final int rangeEnd;
		boolean done = false;
		
		public TestingSourceTokenVisitor(int rangeStart, int rangeEnd) {
			this.rangeStart = rangeStart;
			this.rangeEnd = rangeEnd;
		}

		@Override
		public String visitWhitespace(@NonNull Whitespace tok) {
			return token("ws", tok);
		}

		private String token(String t, HasFileRange tok) {
			assertEquals("Characters skipped?", lastTok != null ? lastTok.getFileRange().getEnd().getOffset() : rangeStart, tok.getFileRange().getStart().getOffset());
			lastTok = tok;
			return t;
		}

		@Override
		public String visitComment(@NonNull Comment tok) {
			return token("com", tok);
		}

		@Override
		public String visitEof(@NonNull FilePos endPos) {
			done = true;
			int fileLength = endPos.getOffset();
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

	}
	@Test public void testTokenStream1() { testScanner("a + b + c + d", 0, 13, new String[] {
			"id", "ws", "op", "ws", "id", "ws", "op", "ws", "id", "ws", "op", "ws", "id", "eof"}); }
	@Test public void testTokenStream2() { testScanner("a + b + c + d", 0, 1, new String[] {"id", "eof"}); }
	@Test public void testTokenStream3() { testScanner("a + b + c + d", 1, 2, new String[] {"ws", "eof"}); }
	@Test public void testTokenStream4() { testScanner("a + b + c + d", 2, 3, new String[] {"op", "eof"}); }
	@Test public void testTokenStream5() { testScanner("/* foo */", 0, 9, new String[] {"com", "eof"}); }
	@Ignore // TODO: Do we need to support scanning partial lines?
	@Test public void testTokenStream6() { testScanner("/* foo */", 1, 5, new String[] {"com", "eof"}); }
	@Test public void testTokenStream7() { testScanner("/* foo */   a", 0, 13, new String[] {"com", "ws", "id", "eof"}); }

	static private void testScanner(@NonNull String src, final int rangeStart, final int rangeEnd, String[] expectedTokens) {
		ParserReader in = ParserReader.fromSubstring("<test>", src, rangeStart, rangeEnd);
		TestingSourceTokenVisitor testVisitor = new TestingSourceTokenVisitor(rangeStart, rangeEnd);
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
