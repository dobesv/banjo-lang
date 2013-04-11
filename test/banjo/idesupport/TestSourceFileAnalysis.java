package banjo.idesupport;

import static banjo.parser.ast.ParseTestUtils.test;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;

import org.junit.Test;

import banjo.parser.BanjoParser;
import banjo.parser.ast.Comment;
import banjo.parser.ast.Expr;
import banjo.parser.ast.HasFileRange;
import banjo.parser.ast.IdRef;
import banjo.parser.ast.ObjectLiteral;
import banjo.parser.ast.OperatorRef;

public class TestSourceFileAnalysis {

	@Test
	public void test1() {
		testTokenizer("// comment\n/* comment */\nfoo: bar", "{foo: bar}", ObjectLiteral.class,
				new String[] {
						"// comment\n",
						"/* comment */",
						"foo", ":",	"bar"
				}, new Class<?>[] {
						Comment.class, Comment.class, 
						IdRef.class, OperatorRef.class, IdRef.class				
				});
	}

	private void testTokenizer(String src, String normalizedSource,
			Class<? extends Expr> expectedClass,
			String[] expectedTokenNormalizedSource,
			Class<?>[] expectedTokenClasses) throws Error {
		final BanjoParser parser = new BanjoParser(src);
		test(src, 0, null, expectedClass, normalizedSource, parser);
		int expectedTokenCount = expectedTokenNormalizedSource.length;
		assertEquals(expectedTokenCount, parser.getTokens().size());
		for(int i=0; i < expectedTokenCount; i++) {
			HasFileRange token = parser.getTokens().get(i);
			assertEquals(expectedTokenClasses[i], token.getClass());
			assertEquals(expectedTokenNormalizedSource[i], token.toString());
		}
	}
	
	static class TestingSourceTokenVisitor implements SourceTokenVisitor<String> {

		int tokenOffset = 0;
		final int rangeEnd;
		final int fileLength;
		boolean done = false;
		
		public TestingSourceTokenVisitor(int rangeStart, int rangeEnd, int fileLength) {
			this.tokenOffset = rangeStart;
			this.rangeEnd = rangeEnd;
			this.fileLength = fileLength;
		}

		@Override
		public String whitespace(int offset, int length) {
			return token("ws", offset, length);
		}

		private String token(String t, int offset, int length) {
			if(offset > 0)
				assertEquals("Characters skipped?", tokenOffset, offset);
			tokenOffset = offset+length;
			return t;
		}

		@Override
		public String comment(int offset, int length) {
			return token("com", offset, length);
		}

		@Override
		public String endOfFile(int fileLength) {
			done = true;
			assertEquals("Wrong end of range?", fileLength, this.fileLength);
			return token("eof", fileLength, 0);
		}

		@Override
		public String operator(int offset, int length) {
			return token("op", offset, length);
		}

		@Override
		public String stringLiteral(int offset, int length) {
			return token("str", offset, length);
		}

		@Override
		public String numberLiteral(int offset, int length) {
			return token("num", offset, length);
		}

		@Override
		public String identifier(int offset, int length,
				EnumSet<IdentifierFlag> flags) {
			return token("id"+(flags.isEmpty()?"":flags.toString()), offset, length);
		}

		@Override
		public String other(int offset, int length) {
			return token("??", offset, length);
		}

		@Override
		public String endOfRange(int rangeEnd) {
			done = true;
			assertEquals("Wrong end of range?", rangeEnd, this.rangeEnd);
			return token("end", rangeEnd, 0);
		}
	}
	@Test public void testTokenStream1() { testTokenStream("a + b + c + d", 0, 13, new String[] {
			"id", "ws", "op", "ws", "id", "ws", "op", "ws", "id", "ws", "op", "ws", "id", "eof"}); }
	@Test public void testTokenStream2() { testTokenStream("a + b + c + d", 0, 1, new String[] {"id", "end"}); }
	@Test public void testTokenStream3() { testTokenStream("a + b + c + d", 1, 2, new String[] {"ws", "end"}); }
	@Test public void testTokenStream4() { testTokenStream("a + b + c + d", 2, 3, new String[] {"op", "end"}); }
	@Test public void testTokenStream5() { testTokenStream("/* foo */", 0, 9, new String[] {"com", "eof"}); }
	@Test public void testTokenStream6() { testTokenStream("/* foo */", 1, 5, new String[] {"com", "eof"}); }
	@Test public void testTokenStream7() { testTokenStream("/* foo */   a", 0, 13, new String[] {"com", "ws", "id", "eof"}); }

	static private void testTokenStream(String src, final int rangeStart,
			final int rangeEnd, String[] expectedTokens) {
		SourceFileAnalysis analysis = new SourceFileAnalysis(src);
		final SourceTokenStream tokenStream = analysis.tokenStream(rangeStart, rangeEnd);
		ArrayList<String> foundTokens = new ArrayList<String>();
		TestingSourceTokenVisitor testVisitor = new TestingSourceTokenVisitor(rangeStart, rangeEnd, src.length());
		while(!testVisitor.done) {
			foundTokens.add(tokenStream.visitNext(testVisitor));
		}
		assertEquals(Arrays.asList(expectedTokens).toString(), foundTokens.toString());
	}
	
}
