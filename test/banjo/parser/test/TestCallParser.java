package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import org.junit.Ignore;
import org.junit.Test;

import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;

public class TestCallParser {

	@Test public void c0() { call("a()", "a()", 0, 0); }
	@Test public void c1() { call("a(b)", "a(b)", 1, 0); }
	@Test public void c2() { call("a(b,c)", "a(b, c)", 2, 0); }
	@Test public void c3() { call("a(b,c,d)", "a(b, c, d)", 3, 0); }
	@Test public void c4() { call("a(b,c,d,e)", "a(b, c, d, e)", 4, 0); }
	@Test public void cnl2() { call("a(b\n  c)", "a(b, c)", 2, 0); }
	@Test public void cnl3() { call("a(b\n  c\n  d)", "a(b, c, d)", 3, 0); }
	@Test public void cnl4() { call("a(b\n  c\n  d\n  e)", "a(b, c, d, e)", 4, 0); }
	@Test public void mixed2() { call("a(b,c\n  d,e)", "a(b, c, d, e)", 4, 0); }
	// TODO Warning/error on mixed comma and semicolon usage
	@Ignore @Test public void mixed3() { call("a(b,c\n  d;e)", "a(b, c, d, e)", 4, 1); }

	@Test public void dedentAfterParen1() { call("a.a().a(\n b\n c\n d\n e)", "a.a.a(b, c, d, e)", 4, 0); }
	@Test public void dedentAfterParen2() { call("a(\nb\nc\nd\ne)", "a(b, c, d, e)", 4, 0); }

	@Test public void testJuxtaposition1() { call("a {foo=1}", "a({foo = 1})", 1); }
	@Test public void testJuxtaposition2() { call("a 1", "a(1)", 1); }
	@Test public void testJuxtaposition3() { call("a \"1\"", "a(\"1\")", 1); }
	@Test public void testJuxtaposition4() { call("a [1,2,3]", "a([1, 2, 3])", 1); }

	@Test public void testJuxtaposition5() { call("a.b {foo=1}", "a.b({foo = 1})", 1); }
	@Test public void testJuxtaposition6() { call("a.b 1", "a.b(1)", 1); }
	@Test public void testJuxtaposition7() { call("a.b \"1\"", "a.b(\"1\")", 1); }
	@Test public void testJuxtaposition8() { call("a.b [1,2,3]", "a.b([1, 2, 3])", 1); }
	@Test public void testJuxtaposition9() { call("a.b {foo=1} (bar)", "a.b({foo = 1})(bar)", 1); }
	@Test public void testJuxtaposition10() { call("a.b {foo=1} {bar}", "a.b({foo = 1})({bar})", 1); }

	@Test public void testMixfixFunc1() { call ("a(1)b(2)c(3)", "a(1)b(2)c(3)", 1); }
	@Test public void testMixfixFunc2() { call ("a 1 b 2 c 3", "a(1)b(2)c(3)", 1); }
	@Test public void testMixfixMethod1() { call ("x.a(1)b(2)c(3)", "x.a(1)b(2)c(3)", 1); }

	@Test public void testPipeTo() { call("x |> y", "y(x)", 1); }
	@Test public void testPipeFrom() { call("x <| y", "x(y)", 1); }

	//	@Test public void objArg1() { call("a(b=1)", "a({b = 1})", 1, 0); }
	//	@Test public void objArg2() { call("a(b = 1,c=2)", "a({b = 1, c = 2})", 1, 0); }
	//	@Test public void objArg3() { call("a(b = 1\n  c = 2)", "a({b = 1, c = 2})", 1, 0); }

	public void call(String source, String expectedSource, int numArgs) {
		call(source, expectedSource, numArgs, 0);
	}
	public void call(String source, String expectedSource, int numArgs, int expectedErrors) {
		ParseTestUtils.test(source, expectedErrors, null, Call.class, expectedSource);
		Call call = (Call)CoreExpr.fromString(source);
		assertEquals(numArgs, call.getArgumentLists().head().length());
	}
}
