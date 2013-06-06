package banjo.dom.test;

import org.junit.Test;

import banjo.desugar.errors.MissingElseClauseInConditional;
import banjo.dom.core.Call;
import banjo.parser.errors.Problem;

public class TestCondParser {

	@Test public void testCond1() { testParseCond("false => true; false", "false.if({true(_): true, false(_): false})"); }
	@Test public void testCond2() { testParseCond("false => true\nfalse", "false.if({true(_): true, false(_): false})"); }
	@Test public void testCond3() { testParseCond("a => b\nc => d\nf", "a.if({true(_): b, false(_): c.if({true(_): d, false(_): f})})"); }
	@Test public void testCond4() { testParseCond("a=>b\nc=>d\ne=>f\nh", "a.if({true(_): b, false(_): c.if({true(_): d, false(_): e.if({true(_): f, false(_): h})})})"); }
	@Test public void testCond5() { testParseCond("   a=>b\n   c=>d\n   e=>f\n   h", "a.if({true(_): b, false(_): c.if({true(_): d, false(_): e.if({true(_): f, false(_): h})})})"); }
	@Test public void testCond6() { testParseCond("a < 0 => -a\na", "a.cmp(0).less().if({true(_): a.negate(), false(_): a})"); }
	@Test public void testCond7() { testParseCond("a=>b\nc", "a.if({true(_): b, false(_): c})"); }

	@Test public void testLazyOr() { testParseCond("a || b", "a.if({false(_): b, true(x): x})"); }
	@Test public void testLazyAnd() { testParseCond("a && b", "a.if({true(_): b, false(x): x})"); }
	@Test public void testLazyAndOr() { testParseCond("a && b || c", "a.if({true(_): b, false(x): x}).if({false(_): c, true(x): x})"); }
	@Test public void testLazyOrAnd() { testParseCond("a || b && c", "a.if({false(_): b.if({true(_): c, false(x): x}), true(x): x})"); }

	@Test public void testLazyOrElse() { testParseCond("a ?: b", "a.valueOrElse((-> b))"); }

	@Test public void testSingleCond() { testParseCond("false => true", MissingElseClauseInConditional.class, 1); }

	public static void testParseCond(String source, String expectedSource) {
		ParseTestUtils.test(source, 0, null, Call.class, expectedSource);
	}
	private void testParseCond(String source, Class<? extends Problem> expectedError, int expectedErrorCount) {
		ParseTestUtils.test(source, expectedErrorCount, expectedError, Call.class, null);
	}

}
