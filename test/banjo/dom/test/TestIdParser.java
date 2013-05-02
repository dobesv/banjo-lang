package banjo.dom.test;

import org.junit.Test;

import banjo.dom.token.Identifier;

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
		ParseTestUtils.test(source, 0, null, Identifier.class, expectedSource);
	}
	
}
