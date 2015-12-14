package banjo.parser.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;

import org.junit.Test;

import banjo.expr.core.CoreExpr;
import banjo.expr.token.StringLiteral;

public class TestStringLiteralParser {

	@Test public void testHelloWorld() { testParser("\"Hello, world!\"", "Hello, world!", 0); }
	@Test public void testEscapes() { testParser("\"\\\"\\t\\n\\r\"", "\"\t\n\r", 0); }

    @Test
    public void testMultiline1() {
        testParser("  \"\n   abc\n   def\n    ghi\n   jkl\n  \"", "abc\ndef\n ghi\njkl\n");
    }
	@Test public void testMultiline2() {

        ParseTestUtils.test(
            "{ x.is positive = (\n    \"\n      Return `true` if this number is positive.\n    \"\n  ) ⇒ !x.negative\n}",
            "{x.is positive = ((_ = \"Return `true` if this number is positive.\\n\") ⇒ ¬x.negative)}");
	}

    @Test
    public void testMultiline3() {
        testParser("  \"\n   abc\n    def\n\n     ghi\n\n    jkl\n   \"", "abc\n def\n\n  ghi\n\n jkl\n");
    }

	// Test line feed handling in string literal
    @Test
    public void testMultiline4() {
        testParser("  \"\r\n   x\r\n\r\n  \"\r\n", "x\n\n");
    }

	@Test public void testBacktick() { testParser("`HelloWorld", "HelloWorld", 0); }
	@Test public void testBacktickEscapes() { testParser("`Hello\\ World\\!", "Hello World!", 0); }
	//@Test public void testBacktickEmpty1() { testParser("`", "", 1); }
	//@Test public void testBacktickEmpty2() { testParser("` ", "", 1); }
	//@Test public void testBacktickEmpty3() { testParser(" ` ", "", 1); }
	@Test public void testBacktickOp1() { testParser("`*", "*", 0); }
	@Test public void testBacktickOp2() { testParser("`\\\\", "\\", 0); }
	@Test public void testEmpty1() { testParser("\"\"", "", 0); }
	@Test public void testEmpty2() { testParser(" \"\" ", "", 0); }


	private void testParser(String source, String expectedString) { testParser(source, expectedString, 0); }

	private void testParser(String sourceString, String expectedParsedString, int expectedErrorCount) {
		ParseTestUtils.test(sourceString, expectedErrorCount, null, StringLiteral.class, null);
		if(expectedErrorCount == 0) {
			StringLiteral actualNode = (StringLiteral) CoreExpr.fromString(sourceString);
			assertNotNull(actualNode);
			assertEquals(expectedParsedString, actualNode.getString());
		}
	}

	@Test
	public void testHexEscapes() throws IOException {
		testParser("\"\\x01\"", "\u0001", 0);
		testParser("\"\\x10\"", "\u0010", 0);
		testParser("\"\\xF0\"", "\u00F0", 0);
		testParser("\"\\xF\"", "", 1);
		testParser("\"\\x\"", "", 1);
		testParser("\"\\x00\"", "\u0000", 0);

		testParser("\"\\u0001\"", "\u0001", 0);
		testParser("\"\\u0010\"", "\u0010", 0);
		testParser("\"\\u00F0\"", "\u00F0", 0);
		testParser("\"\\u2022\"", "\u2022", 0);
		testParser("\"\\u00F\"", "", 1);
		testParser("\"\\u00\"", "", 1);
		testParser("\"\\u0000\"", "\u0000", 0);
	}

}
