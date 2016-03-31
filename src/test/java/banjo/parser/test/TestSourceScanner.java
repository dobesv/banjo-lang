package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.test;
import static banjo.parser.test.TokensToString.testScanner;
import static org.junit.Assert.assertEquals;

import org.junit.Ignore;
import org.junit.Test;

import banjo.expr.Expr;
import banjo.expr.core.Let;
import banjo.expr.source.SourceExprFactory;
import banjo.expr.token.Comment;
import banjo.expr.token.Identifier;
import banjo.expr.token.OperatorRef;
import banjo.expr.token.Token;
import banjo.expr.token.TokenScanner;
import banjo.expr.token.Whitespace;
import banjo.expr.util.TokenCollector;
import fj.data.List;
public class TestSourceScanner {
	@Test
	public void test1() {
		testTokenizer("# comment\n(foo= bar bar) =>\nbaz\n", "(foo = bar bar) ⇒ baz", Let.class,
				new String[] {
			"# comment\n",
			"(", "foo", "=",	" ", "bar bar", ")", " ", "=>", "\n",
			"baz", "\n"
		}, new Class<?>[] {
			Comment.class,
			OperatorRef.class, Identifier.class, OperatorRef.class, Whitespace.class, Identifier.class, OperatorRef.class, Whitespace.class, OperatorRef.class, Whitespace.class,
			Identifier.class, Whitespace.class
		});
	}

	private void testTokenizer(String src, String normalizedSource,
			Class<? extends Expr> expectedClass,
			String[] expectedTokenNormalizedSource,
			Class<?>[] expectedTokenClasses) throws Error {
		final TokenScanner scanner = new TokenScanner();
		List<Token> tokens = scanner.scan(src, new TokenCollector()).getTokens();
		final SourceExprFactory parser = new SourceExprFactory();
		test(src, 0, null, expectedClass, normalizedSource, parser);
		final int expectedTokenCount = expectedTokenNormalizedSource.length;
		assertEquals(expectedTokenCount, tokens.length());
		for(int i=0; i < expectedTokenCount; i++, tokens = tokens.tail()) {
			final Token token = tokens.head();
			assertEquals(expectedTokenClasses[i], token.getClass());
			assertEquals(expectedTokenNormalizedSource[i], token.toString());
		}
	}

	@Test public void testTokenStream1() { testScanner("a + b + c + d", 0, 13,
			"id", "ws", "op", "ws", "id", "ws", "op", "ws", "id", "ws", "op", "ws", "id"); }
	@Test public void testTokenStream2() { testScanner("a + b + c + d", 0, 1, "id"); }
	@Test public void testTokenStream3() { testScanner("a + b + c + d", 1, 2, "ws"); }
	@Test public void testTokenStream4() { testScanner("a + b + c + d", 2, 3, "op"); }
	@Test public void testTokenStream5() { testScanner("# foo", 0, 5, "com"); }
	@Ignore // TODO: Do we need to support scanning partial lines?
	@Test public void testTokenStream6() { testScanner("/* foo */", 1, 5, "com"); }
	@Test public void testTokenStream7() { testScanner("#  foo  \n   a", 0, 13, "com", "ws", "id"); }
	@Test public void testTokenStream8() { testScanner("a?,b?", 0, 5, "id", "op", "op", "id", "op"); }

}
