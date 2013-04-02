package banjo.parser.ast;

import org.junit.Test;

public class TestMathParser {

	@Test public void test1() { test("1 + 2 * 3", "(1).plus((2).times(3))"); }
	@Test public void test2() { test("3 * 2 + 1", "(3).times(2).plus(1)"); }
	
	@Test public void test3() { test("2 + 2 > 1 + 1", "(2).plus(2).gt((1).plus(1))");
	}
	
	@Test public void unaries1() throws Exception { unaries("+ - ~1"); }
	@Test public void unaries2() throws Exception { unaries("+\n -\n  ~\n   1"); }
	@Test public void unaries3() throws Exception { unaries("  +  -  ~ 1"); }

	public void unaries(String src) {
		test(src, "(1).complement().negate().plus()");
	}
	
	public static void test(String source, String normalizedSource) {
		ParseTestUtils.testParse(source, 0, Call.class, normalizedSource);
	}

	
}
