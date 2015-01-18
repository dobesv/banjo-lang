package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.test;

import org.junit.Test;

import banjo.dom.core.ObjectLiteral;

public class TestSelectorParser {
	public static void parse(String source, String expectedSource) {
		test(source, 0, null, ObjectLiteral.class, expectedSource);
	}

	@Test public void test1() { parse(".foo", ".foo"); }
	@Test public void test2() { parse(".foo(x)", ".foo(x)"); }
	@Test public void test3() { parse(".(x)", ".(x)"); }
	@Test public void test4() { parse(".(x + y)", ".(x + y)"); }
}
