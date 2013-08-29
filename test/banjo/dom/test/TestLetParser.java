package banjo.dom.test;

import static banjo.dom.test.ParseTestUtils.test;

import org.junit.Test;

import banjo.dom.source.BadSourceExpr.ExpectedOperator;

public class TestLetParser {
	private static final String HELLO_WORLD_NORMALIZED = "{(hello) = hello}(\"world\")";

	@Test public void oneLine()         { test("hello = \"world\" ; hello", HELLO_WORLD_NORMALIZED); }
	@Test public void twoLine()         { test("hello = \"world\"\nhello", HELLO_WORLD_NORMALIZED); }
	@Test public void twoLineIndented() { test("   hello = \"world\"\n   hello", HELLO_WORLD_NORMALIZED); }
	@Test public void badBackdent()     { test("   hello = \"world\"\nhello", 1, ExpectedOperator.class, null, null); } // Backdent here should be reported as an error
	// TODO Check indentation
	@Test public void badIndent()       { test(" hello = \"world\"\n   hello", 1, ExpectedOperator.class, null, null); } // Indent here should be reported as an error

	@Test public void f1() { test("f(x) = x ; f(0)", "{(f) = f(0)}({(x) = x})"); }
	@Test public void f2() { test("f(x,y) = x ; f(1,2)", "{(f) = f(1, 2)}({(x, y) = x})"); }
	@Test public void f3() { test("f() = x ; f()", "{(f) = f()}({() = x})"); }

}
