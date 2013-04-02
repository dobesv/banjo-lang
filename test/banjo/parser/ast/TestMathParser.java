package banjo.parser.ast;

import org.junit.Test;

public class TestMathParser {

	@Test public void test1() { test("1 + 2 * 3", "Math.add(1, Math.mul(2, 3))"); }
	@Test public void test2() { test("3 * 2 + 1", "Math.add(Math.mul(3, 2), 1)"); }
	
	@Test public void test3() { test("2 + 2 > 1 + 1", "Math.gt(Math.add(2, 2), Math.add(1, 1))");
	}
	
	@Test public void unaries1() throws Exception { unaries("+ - ~1"); }
	@Test public void unaries2() throws Exception { unaries("+\n -\n  ~\n   1"); }
	@Test public void unaries3() throws Exception { unaries("  +  -  ~ 1"); }

	public void unaries(String src) {
		test(src, "1 .complement().negate().plus()");
	}
	
	public static void test(String source, String normalizedSource) {
		ParseTestUtils.testParse(source, 0, Call.class, normalizedSource);
	}

	
}
