package banjo.parser.ast;

import org.junit.Test;

public class TestIdParser {

	@Test public void abc() { test("abc"); }
	@Test public void abcCaps() { test("ABC"); }
	@Test public void testSpecialChars1() { test("\\."); }
	@Test public void testSpecialChars2() { test("\\-\\-"); }
	@Test public void testSpecialChars3() { test("\\-\\ \\-"); }

	private void test(String string) {
		test(string,string);
	}

	private void test(String source, String expectedSource) {
		ParseTestUtils.testParse(source, 0, null, IdRef.class, expectedSource);
	}
	
}
