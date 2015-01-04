package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.test;

import org.junit.Test;

import banjo.dom.source.BadSourceExpr.ExpectedOperator;

public class TestLetParser {
	private static final String HELLO_WORLD_NORMALIZED = "((hello) -> hello)(\"world\")";

	@Test public void oneLine()         { test("( hello = \"world\" ) => hello", HELLO_WORLD_NORMALIZED); }
	@Test public void twoLine1()         { test("(hello = \"world\"\n)=>hello", HELLO_WORLD_NORMALIZED); }
	@Test public void twoLine2()         { test("(hello = \"world\"\n )=>hello", HELLO_WORLD_NORMALIZED); }
//	@Test public void twoLineIndented() { test("   (hello = \"world\")\n   => hello", HELLO_WORLD_NORMALIZED); }
//	@Test public void badBackdent()     { test("   (hello = \"world\") =>\nhello", 1, ExpectedOperator.class, null, null); } // Backdent here should be reported as an error
//	@Test public void badIndent()       { test(" (hello = \"world\")\n   => hello", 1, ExpectedOperator.class, null, null); } // Indent here should be reported as an error

	@Test public void foo1() { test("(a = foo) => a", "((a) -> a)(foo)"); }
	@Test public void foo2() { test("(a = b) => c", "((a) -> c)(b)"); }
	@Test public void foo3() { test("(a = foo\n) => a", "((a) -> a)(foo)"); }
	@Test public void foo5() { test("(\n a = foo\n b = bar\n) => a\n", "((a) -> ((b) -> a)(bar))(foo)"); }

	@Test public void f1() { test("(f(x) = x) => f(0)", "((f) -> f(0))(f(x) -> x)"); }
	@Test public void f2() { test("(f(x,y) = x) => f(1,2)", "((f) -> f(1, 2))(f(x, y) -> x)"); }
	@Test public void f3() { test("(f() = x) => f()", "((f) -> f())(f() -> x)"); }

	@Test public void multiline1() { test("{\n  x = (\n    doc='foo'\n  ) => bar\n}", "{x = ((doc) -> bar)(\"foo\")}"); }
	@Test public void multiline2() { test("{\n  x = (\n    doc='foo'\n  ) => bar\n}", "{x = ((doc) -> bar)(\"foo\")}"); }
	@Test public void multiline3() { test("{\n  x = (\n    doc='foo'\n    examples=[]\n  ) => bar\n}", "{x = ((doc) -> ((examples) -> bar)([]))(\"foo\")}"); }
}
