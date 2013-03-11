package banjo.parser.ast;

import org.junit.Test;
import static org.junit.Assert.*;

public class TestCallParser {

	@Test public void c0() { call("a()", "a()", 0, 0); }
	@Test public void c1() { call("a(b)", "a(b)", 1, 0); }
	@Test public void c2() { call("a(b,c)", "a(b, c)", 2, 0); }
	@Test public void c3() { call("a(b,c,d)", "a(b, c, d)", 3, 0); }
	@Test public void c4() { call("a(b,c,d,e)", "a(b, c, d, e)", 4, 0); }
	@Test public void cnl2() { call("a(b\n  c)", "a(b, c)", 2, 0); }
	@Test public void cnl3() { call("a(b\n  c\n  d)", "a(b, c, d)", 3, 0); }
	@Test public void cnl4() { call("a(b\n  c\n  d\n  e)", "a(b, c, d, e)", 4, 0); }
	@Test public void mixed1() { call("a(b\n  c;d)", "a(b, c, d)", 3, 0); }
	@Test public void mixed2() { call("a(b,c\n  d,e)", "a(b, c, d, e)", 4, 0); }
	@Test public void mixed3() { call("a(b,c\n  d;e)", "a(b, c, d, e)", 4, 1); }
	
	public void call(String source, String expectedSource, int numArgs, int expectedErrors) {
		Call call = ParseTestUtils.testParse(source, expectedErrors, Call.class, expectedSource);
		assertEquals(numArgs, call.getArguments().size());
	}
}
