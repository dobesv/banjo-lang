package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

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

}
