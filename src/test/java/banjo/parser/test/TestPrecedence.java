package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import org.junit.Ignore;
import org.junit.Test;

import banjo.expr.source.SourceExpr;

public class TestPrecedence {

	@Test public void newlinePrecAssoc() { test("a=1\nb=2\nc", "(a = 1) (nl) ((b = 2) (nl) c)"); }
	@Test public void commaPrecAssoc() { test("a=1,b=2,c", "(a = 1) , ((b = 2) , c)"); }
	@Test public void functionLiteralContainsExtend() {
		test("a -> b @ c", "a ↦ (b Φ c)");
	}
	@Test public void extendContainsFunctionLiteral() {
		test("a @ b -> c", "a Φ (b ↦ c)");
	}
	private void test(String source, String expectedFullyParenthesized) {
		final SourceExpr parseResult = SourceExpr.fromString(source);
		final String actualFullyParenthesizedSource = parseResult.toFullyParenthesizedSource();
		assertEquals(expectedFullyParenthesized, actualFullyParenthesizedSource);
	}

	@Ignore @Test public void testDedentClosesParen() {
		// De-dent should close the paren to help the error checker "recover" from missing close parens; BUT only if the de-dent goes past the start of the line with
		// the open paren, it's  OK to de-dent past the open paren if it wasn't the first thing on its line.
		test("   x = (\na + b\n) ; x", "");
	}

}
