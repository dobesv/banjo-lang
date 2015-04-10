package banjo.parser.test;

import org.junit.Ignore;
import org.junit.Test;

import banjo.dom.BadExpr;
import banjo.dom.source.BadSourceExpr;

public class TestMathParser {

	@Test public void t1() { test("1 + 2 × 3", "1 + 2 × 3"); }
	@Test public void t2() { test("3 * 2 + 1", "3 × 2 + 1"); }
	@Test public void t3() { test("2 + 2 > 1 + 1", "2 + 2 > 1 + 1"); }
	@Test public void t4() { test("2 + 2 >\n1 + 1", "2 + 2 > 1 + 1"); }
	@Test public void t5() { test("2 +\n2 >\n1 +\n1", "2 + 2 > 1 + 1"); }

	@Test public void multiline1() { test("2 + 2 >\n 1 +\n 1", "2 + 2 > 1 + 1"); }
	//@Test public void multiline2() { test("2 + 2\n > 1 + 1", "2 + 2 > 1 + 1"); }
	//@Test public void multiline3() { test("2\n + 2\n  > 1\n   + 1", "2 + 2 > 1 + 1"); }
	//@Test public void multiline4() { test("  2\n+ 2", "2 + 2"); }

	// Note how indentation is overriding the operator precedence here; feature or bug ?
	//@Test public void multiline5() { test("  2\n+ 2\n× 4\n", "(2 + 2) × 4"); }

	@Test public void gt() { test("3 > 2", "3 > 2"); }
	@Test public void lt() { test("3 < 2", "3 < 2"); }
	@Test public void ge() { test("3 >= 2", "3 ≥ 2"); }
	@Test public void le() { test("3 <= 2", "3 ≤ 2"); }

	@Test public void extend() { test("a @ b", "a @ b"); }

	// TODO Support right-aligned operands
	@Ignore @Test public void testRightAlign() { test("   1\n+ 11\n+111", "(1).\\+(11).\\+(111)"); }

	@Test public void badDedent1() { test("    3\n> 2", 1, BadSourceExpr.class); }
	@Test public void badDedent2() { test("    3\n@ 2", 1, BadSourceExpr.class); }

	@Test public void unaries1() { unaries("+ - ~1"); }
	@Test public void unaries2() { unaries("+\n -\n  ~\n   1"); }
	@Test public void unaries3() { unaries("  +  -  ~ 1"); }

	@Test public void plusOne1() { test("x+1", "x + 1"); }
	@Test public void plusOne2() { test("+1+x", "+1 + x"); }
	@Test public void minusOne1() { test("x-1", "x - 1"); }
	@Test public void minusOne2() { test("-1-x", "-1 - x"); }

	@Test public void abs1() { test("|x|", "|x|"); }

	@Test public void memberOf1() { test("x in y", "x in y"); }
	@Test public void memberOf2() { test("x\u2208y", "x ∈ y"); }

	@Test public void parenMultiline1() { test("{\n  x = (\n    doc = \"bla\"\n  ) => bloo(\n    1, 2, 3\n  )\n}", "{x = ((doc = \"bla\") => bloo(1, 2, 3))}"); }

	@Test public void negateCallResult() { test("-abs(x)", "-abs(x)"); }

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
