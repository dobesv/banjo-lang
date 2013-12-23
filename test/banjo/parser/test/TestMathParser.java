package banjo.parser.test;

import org.junit.Ignore;
import org.junit.Test;

import banjo.dom.BadExpr;
import banjo.dom.source.BadSourceExpr.UnsupportedOperator;

public class TestMathParser {

	@Test public void t1() { test("1 + 2 * 3", "1 + 2 * 3"); }
	@Test public void t2() { test("3 * 2 + 1", "3 * 2 + 1"); }
	@Test public void t3() { test("2 + 2 > 1 + 1", "2 + 2 > 1 + 1"); }
	@Test public void t4() { test("2 + 2 >\n1 + 1", "2 + 2 > 1 + 1"); }
	@Test public void t5() { test("2 +\n2 >\n1 +\n1", "2 + 2 > 1 + 1"); }

	@Test public void multiline1() { test("2 + 2 >\n 1 +\n 1", "2 + 2 > 1 + 1"); }
	@Test public void multiline2() { test("2 + 2\n > 1 + 1", "2 + 2 > 1 + 1"); }
	@Test public void multiline3() { test("2\n + 2\n  > 1\n   + 1", "2 + 2 > 1 + 1"); }
	@Test public void multiline4() { test("  2\n+ 2", "2 + 2"); }

	// Not how indentation is overriding the operator precedence here; feature or bug ?
	@Test public void multiline5() { test("  2\n+ 2\n* 4", "(2 + 2) * 4"); }

	@Test public void gt() { test("3 > 2", "3 > 2"); }
	@Test public void lt() { test("3 < 2", "3 < 2"); }
	@Test public void ge() { test("3 >= 2", "3 >= 2"); }
	@Test public void le() { test("3 <= 2", "3 <= 2"); }

	@Test public void extend() { test("a {+} b", "a{+}b"); }

	// TODO Support right-aligned operands
	@Ignore @Test public void testRightAlign() { test("   1\n+ 11\n+111", "(1).\\+(11).\\+(111)"); }

	@Test public void badDedent1() { test("3\n> 2", 1, UnsupportedOperator.class); }
	@Test public void badDedent2() { test("3\n@@ 2", 1, UnsupportedOperator.class); }

	@Test public void unaries1() { unaries("+ - ~1"); }
	@Test public void unaries2() { unaries("+\n -\n  ~\n   1"); }
	@Test public void unaries3() { unaries("  +  -  ~ 1"); }

	@Test public void questionMark() { test("s?", "s?"); }
	@Test public void existential() { test("s??", "s??"); }

	@Test public void semicolon1() { test("a ; b", "a ; b"); }
	@Test public void semicolon2() { test("a = 1 ; b", "((a) -> b)(1)"); }
	@Ignore @Test public void semicolon3() { test("{ foo = bar = 1; bar+1 }", ""); }
	@Test public void semicolon3b() { test("{ foo = (bar = 1; bar+1) }", "{foo = ((bar) -> bar + 1)(1)}"); }

	@Test public void plusOne1() { test("x+1", "x + 1"); }
	@Test public void plusOne2() { test("+1+x", "+1 + x"); }
	@Test public void minusOne1() { test("x-1", "x - 1"); }
	@Test public void minusOne2() { test("-1-x", "-1 - x"); }

	@Test public void abs1() { test("|x|", "|x|"); }


	public void unaries(String src) {
		test(src, "+-~1");
	}

	public static void test(String source, String normalizedSource) {
		ParseTestUtils.test(source, 0, null, null, normalizedSource);
	}
	private void test(String source, int expectedErrorCount, Class<? extends BadExpr> expectedError) {
		ParseTestUtils.test(source, expectedErrorCount, expectedError, null, null);
	}


}
