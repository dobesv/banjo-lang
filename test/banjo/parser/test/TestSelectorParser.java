package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.test;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import banjo.dom.core.CoreExpr;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.ObjectLiteral;

public class TestSelectorParser {
	public static void parse(String source, String expectedSource) {
		test(source, 0, null, FunctionLiteral.class, expectedSource);
		FunctionLiteral obj = (FunctionLiteral)CoreExpr.fromString(source);
		assertTrue(obj.isSelector());
	}

	@Test public void test1() { parse(".foo", ".foo"); }
	@Test public void test2() { parse(".foo(x)", ".foo(x)"); }
	@Test public void test3() { parse(".(x)", ".(x)"); }
	@Test public void test4() { parse(".(x + y)", ".(x + y)"); }
}
