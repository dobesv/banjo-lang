package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.test;
import static banjo.parser.test.TokensToString.testScanner;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;

import org.eclipse.jdt.annotation.NonNull;
import org.junit.Ignore;
import org.junit.Test;

import banjo.dom.Expr;
import banjo.dom.core.Call;
import banjo.dom.token.Comment;
import banjo.dom.token.Identifier;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.Token;
import banjo.dom.token.Whitespace;
import banjo.parser.BanjoParser;
import banjo.parser.BanjoScanner;
import banjo.parser.util.TokenCollector;
public class TestBanjoScanner {
	@Test
	public void test1() {
		testTokenizer("// comment\n/* comment */\nfoo= bar\nbaz\n", "((foo) -> baz)(bar)", Call.class,
				new String[] {
			"// comment\n",
			"/* comment */", "\n",
			"foo", "=",	" ", "bar", "\n",
			"baz", "\n"
		}, new Class<?>[] {
			Comment.class,
			Comment.class, Whitespace.class,
			Identifier.class, OperatorRef.class, Whitespace.class, Identifier.class, Whitespace.class,
			Identifier.class, Whitespace.class
		});
	}

	private void testTokenizer(@NonNull String src, String normalizedSource,
			Class<? extends Expr> expectedClass,
			String[] expectedTokenNormalizedSource,
			Class<?>[] expectedTokenClasses) throws Error {
		final BanjoScanner scanner = new BanjoScanner();
		final ArrayList<Token> tokens = new ArrayList<>();
		scanner.scan(src, new TokenCollector(new BanjoParser(), tokens));
		final BanjoParser parser = new BanjoParser();
		test(src, 0, null, expectedClass, normalizedSource, parser);
		final int expectedTokenCount = expectedTokenNormalizedSource.length;
		assertEquals(expectedTokenCount, tokens.size());
		for(int i=0; i < expectedTokenCount; i++) {
			final Token token = tokens.get(i);
			assertEquals(expectedTokenClasses[i], token.getClass());
			assertEquals(expectedTokenNormalizedSource[i], token.toString());
		}
	}

	@Test public void testTokenStream1() { testScanner("a + b + c + d", 0, 13,
			"id", "ws", "op", "ws", "id", "ws", "op", "ws", "id", "ws", "op", "ws", "id", "eof"); }
	@Test public void testTokenStream2() { testScanner("a + b + c + d", 0, 1, "id", "eof"); }
	@Test public void testTokenStream3() { testScanner("a + b + c + d", 1, 2, "ws", "eof"); }
	@Test public void testTokenStream4() { testScanner("a + b + c + d", 2, 3, "op", "eof"); }
	@Test public void testTokenStream5() { testScanner("/* foo */", 0, 9, "com", "eof"); }
	@Ignore // TODO: Do we need to support scanning partial lines?
	@Test public void testTokenStream6() { testScanner("/* foo */", 1, 5, "com", "eof"); }
	@Test public void testTokenStream7() { testScanner("/* foo */   a", 0, 13, "com", "ws", "id", "eof"); }
	@Test public void testTokenStream8() { testScanner("a?,b?", 0, 5, "id", "op", "op", "id", "op", "eof"); }

}
