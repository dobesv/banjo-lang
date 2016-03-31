package banjo.parser.test;

import static banjo.parser.test.ParseTestUtils.test;

import org.junit.Test;

public class TestLetParser {
    private static final String HELLO_WORLD_NORMALIZED = "(hello = \"world\") ⇒ hello";

	@Test public void oneLine()         { test("( hello = \"world\" ) => hello", HELLO_WORLD_NORMALIZED); }
	@Test public void twoLine1()         { test("(hello = \"world\"\n)=>hello", HELLO_WORLD_NORMALIZED); }

    @Test
    public void twoLine2() {
        test("(\n hello = \"world\"\n)=>hello", HELLO_WORLD_NORMALIZED);
    }
//	@Test public void twoLineIndented() { test("   (hello = \"world\")\n   => hello", HELLO_WORLD_NORMALIZED); }
//	@Test public void badBackdent()     { test("   (hello = \"world\") =>\nhello", 1, ExpectedOperator.class, null, null); } // Backdent here should be reported as an error
//	@Test public void badIndent()       { test(" (hello = \"world\")\n   => hello", 1, ExpectedOperator.class, null, null); } // Indent here should be reported as an error

    @Test
    public void foo1() {
        test("(a = foo) => a", "(a = foo) ⇒ a");
    }

    @Test
    public void foo2() {
        test("(a = b) ⇒ c", "(a = b) ⇒ c");
    }

    @Test
    public void foo3() {
        test("(a = foo\n) => a", "(a = foo) ⇒ a");
    }

    @Test
    public void foo5() {
        test("(\n a = foo\n b = bar\n) => a\n", "(b = bar, a = foo) ⇒ a");
    }

    @Test
    public void f1() {
        test("(f(x) = x) => f(0)", "(f(x) = x) ⇒ f(0)");
    }

    @Test
    public void f2() {
        test("(f(x,y) = x) => f(1,2)", "(f(x, y) = x) ⇒ f(1, 2)");
    }

    @Test
    public void f3() {
        test("(f() = x) => f()", "(f() = x) ⇒ f()");
    }

    @Test
    public void mixfix1() {
        test("(f(x)g(y) = x + y) => f(1)g(2)", "(f _ g(x) = (y) ↦ x + y) ⇒ f _ g(1)(2)");
    }

    @Test
    public void multiline1() {
        test("{\n  x = (\n    doc='foo'\n  ) => bar\n}", "{x = ((doc = \"foo\") ⇒ bar)}");
    }

    @Test
    public void multiline2() {
        test("{\n  x = (\n    doc='foo'\n  ) => bar\n}", "{x = ((doc = \"foo\") ⇒ bar)}");
    }

    @Test
    public void multiline3() {
        test("{\n  x = (\n    doc='foo'\n    examples=[]\n  ) => bar\n}", "{x = ((examples = [], doc = \"foo\") ⇒ bar)}");
    }

    @Test
    public void testDocString() {
        test("{ x = (\"bla\") => true }", "{x = ((_ = \"bla\") ⇒ true)}");
    }

    @Test
    public void testDocString2() {
        test("{ x = (\n  \"bla\"\n  foo = foo\n) => true\n}", "{x = ((foo = foo, _ = \"bla\") ⇒ true)}");
    }

    @Test
    public void noParensDocString1() {
        test("\"bla\" => true", "(_ = \"bla\") ⇒ true");
    }

    @Test
    public void noParensDocString2() {
        test("\"bla\" =>\ntrue", "(_ = \"bla\") ⇒ true");
    }
}
