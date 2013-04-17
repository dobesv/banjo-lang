package banjo.parser.test;

import static banjo.dom.test.ParseTestUtils.test;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;

import org.eclipse.jdt.annotation.NonNull;
import org.junit.Ignore;
import org.junit.Test;

import banjo.dom.Comment;
import banjo.dom.Expr;
import banjo.dom.HasFileRange;
import banjo.dom.Identifier;
import banjo.dom.ObjectLiteral;
import banjo.dom.OperatorRef;
import banjo.dom.Whitespace;
import banjo.parser.BanjoParser;
import banjo.parser.BanjoScanner;
import banjo.parser.util.TokenCollector;
import static banjo.parser.test.TokensToString.testScanner;
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
	
	@Test public void testTokenStream1() { testScanner("a + b + c + d", 0, 13, 
			"id", "ws", "op", "ws", "id", "ws", "op", "ws", "id", "ws", "op", "ws", "id", "eof"); }
	@Test public void testTokenStream2() { testScanner("a + b + c + d", 0, 1, "id", "eof"); }
	@Test public void testTokenStream3() { testScanner("a + b + c + d", 1, 2, "ws", "eof"); }
	@Test public void testTokenStream4() { testScanner("a + b + c + d", 2, 3, "op", "eof"); }
	@Test public void testTokenStream5() { testScanner("/* foo */", 0, 9, "com", "eof"); }
	@Ignore // TODO: Do we need to support scanning partial lines?
	@Test public void testTokenStream6() { testScanner("/* foo */", 1, 5, "com", "eof"); }
	@Test public void testTokenStream7() { testScanner("/* foo */   a", 0, 13, "com", "ws", "id", "eof"); }

}
