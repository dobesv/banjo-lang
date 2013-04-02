package banjo.parser.ast;

import org.junit.Assert;
import org.junit.Test;

import banjo.parser.errors.BanjoParseException;
import banjo.parser.errors.IncorrectIndentation;
import banjo.parser.errors.UnexpectedDecimalPoint;
import banjo.parser.errors.UnsupportedUnaryOperator;

public class TestMathParser {

	@Test public void test1() { test("1 + 2 * 3", "(1).plus((2).times(3))"); }
	@Test public void test2() { test("3 * 2 + 1", "(3).times(2).plus(1)"); }
	@Test public void test3() { test("2 + 2 > 1 + 1", "(2).plus(2).gt((1).plus(1))"); }
	
	@Test public void testMultiline1() { test("2 + 2 >\n 1 +\n 1", "(2).plus(2).gt((1).plus(1))"); }
	@Test public void testMultiline2() { test("2 + 2\n > 1 + 1", "(2).plus(2).gt((1).plus(1))"); }
	@Test public void testMultiline3() { test("2\n + 2\n  > 1\n   + 1", "(2).plus(2).gt((1).plus(1))"); }
	
	@Test public void badDedent1() { test("3\n> 2", UnsupportedUnaryOperator.class); }
	
	@Test public void unaries1() throws Exception { unaries("+ - ~1"); }
	@Test public void unaries2() throws Exception { unaries("+\n -\n  ~\n   1"); }
	@Test public void unaries3() throws Exception { unaries("  +  -  ~ 1"); }

	public void unaries(String src) {
		test(src, "(1).complement().negate().plus()");
	}
	
	public static void test(String source, String normalizedSource) {
		ParseTestUtils.testParse(source, 0, null, Call.class, normalizedSource);
	}
	private void test(String source, Class<? extends BanjoParseException> expectedError) {
		ParseTestUtils.testParse(source, 2, expectedError, null, null);
	}

	
}
