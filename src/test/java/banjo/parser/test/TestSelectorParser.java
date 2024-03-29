package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.test;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import banjo.expr.core.CoreExpr;
import banjo.expr.core.BindingExpr;

public class TestSelectorParser {
	public static void parse(String source, String expectedSource) {
		test(source, 0, null, BindingExpr.class, expectedSource);
	}

	@Test
	public void test1() {
		parse(".foo", ".foo");
	}

	@Test
	public void test2() {
		parse(".foo(x)", ".foo(x)");
	}

	@Test
	public void test3() {
		parse(".(x)", ".x");
	}

	@Test
	public void test4() {
		parse(".(x + y)", ".(x + y)");
	}

	@Test
	public void test5() {
		parse(".foo(x,y)", ".foo(x, y)");
	}

	@Test
	public void testOpPlus() {
		parse(".+(7)", ".\\+(7)");
	}

	@Test
	public void testOpMinus() {
		parse(".-(7)", ".\\-(7)");
	}

	@Test
	public void testOpTimesJuxta() {
		parse(".* 7", ".\\×(7)");
	}

	@Test
	public void testOpDivJuxta() {
		parse("./ 7", ".\\÷(7)");
	}

	@Test
	public void testOpNot() {
		parse(".!", ".\\¬");
	}

	@Test
	public void testOpInv() {
		parse(".~", ".\\~");
	}

}
