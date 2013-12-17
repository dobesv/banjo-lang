package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Ignore;
import org.junit.Test;

import banjo.parser.BanjoParser;
import banjo.parser.BanjoParser.ExtSourceExpr;
import banjo.parser.util.UnexpectedIOExceptionError;

public class TestPrecedence {

	@Test public void testNewlinePrecAssoc() { test("a=1\nb=2\nc", "(a = 1) (nl) ((b = 2) (nl) c)"); }
	@SuppressWarnings("null")

	private void test(String source, String expectedFullyParenthesized) {
		try {
			final ExtSourceExpr parseResult = new BanjoParser().parse(source);
			final String actualFullyParenthesizedSource = parseResult.getExpr().toFullyParenthesizedSource();
			assertEquals(expectedFullyParenthesized, actualFullyParenthesizedSource);
		} catch (final IOException e) {
			throw new UnexpectedIOExceptionError(e);
		}
	}

	@Ignore @Test public void testDedentClosesParen() {
		// De-dent should close the paren to help the error checker "recover" from missing close parens; BUT only if the de-dent goes past the start of the line with
		// the open paren, it's  OK to de-dent past the open paren if it wasn't the first thing on its line.
		test("   x = (\na + b\n) ; x", "");
	}

}
