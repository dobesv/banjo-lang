package banjo.dom.test;

import org.junit.Test;

import banjo.desugar.errors.MissingElseClauseInConditional;
import banjo.dom.Call;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.errors.ExpectedOperator;

public class TestCondParser {

	@Test public void testCond1() { testParseCond("false => true; ... => false", "false.if({true: () -> true, false: () -> false})"); }
	@Test public void testCond2() { testParseCond("false => true\n... => false", "false.if({true: () -> true, false: () -> false})"); }
	@Test public void testCond3() { testParseCond("a => b\nc => d\nf", "a.if({true: () -> b, false: () -> c.if({true: () -> d, false: () -> f})})"); }
	@Test public void testCond4() { testParseCond("a=>b\nc=>d\ne=>f\nh", "a.if({true: () -> b, false: () -> c.if({true: () -> d, false: () -> e.if({true: () -> f, false: () -> h})})})"); }
	@Test public void testCond5() { testParseCond("   a=>b\n   c=>d\n   e=>f\n   h", "a.if({true: () -> b, false: () -> c.if({true: () -> d, false: () -> e.if({true: () -> f, false: () -> h})})})"); }
	@Test public void testCond6() { testParseCond("a < 0 => -a\n... => a", "a.cmp(0).less().if({true: () -> a.negate(), false: () -> a})"); }
	@Test public void testCond7() { testParseCond("a=>b\nc", "a.if({true: () -> b, false: () -> c})"); }
	
	@Test public void testLazyOr() { testParseCond("a || b", "a.if({false: () -> b, true: () -> false})"); }
	@Test public void testLazyAnd() { testParseCond("a && b", "a.if({true: () -> b, false: () -> false})"); }
	@Test public void testLazyAndOr() { testParseCond("a && b || c", "a.if({true: () -> b, false: () -> false}).if({false: () -> c, true: () -> false})"); }
	@Test public void testLazyOrAnd() { testParseCond("a || b && c", "a.if({false: () -> b.if({true: () -> c, false: () -> false}), true: () -> false})"); }

	@Test public void testLazyOrElse() { testParseCond("a ?: b", "a.valueOrElse((() -> b))"); }
	
	@Test public void testSingleCond() { testParseCond("false => true", MissingElseClauseInConditional.class, 1); }
	
	public static void testParseCond(String source, String expectedSource) {
		ParseTestUtils.test(source, 0, null, Call.class, expectedSource);
	}
	private void testParseCond(String source, Class<? extends BanjoParseException> expectedError, int expectedErrorCount) {
		ParseTestUtils.test(source, expectedErrorCount, expectedError, Call.class, null);
	}
	
}
