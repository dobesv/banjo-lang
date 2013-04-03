package banjo.parser.ast;

import org.junit.Test;

import banjo.parser.errors.BanjoParseException;
import banjo.parser.errors.ExpectedOperator;
import banjo.parser.errors.MissingElseClauseInConditional;

public class TestCondParser {

	@Test public void testCond1() { testParseCond("false => true; ... => false", "false.lazyIfTrue((() -> true), (() -> false))"); }
	@Test public void testCond2() { testParseCond("false => true\n... => false", "false.lazyIfTrue((() -> true), (() -> false))"); }
	@Test public void testCond3() { testParseCond("a => b\nc => d\nf", "a.lazyIfTrue((() -> b), (() -> c.lazyIfTrue((() -> d), (() -> f))))"); }
	@Test public void testCond4() { testParseCond("a=>b\nc=>d\ne=>f\nh", "a.lazyIfTrue((() -> b), (() -> c.lazyIfTrue((() -> d), (() -> e.lazyIfTrue((() -> f), (() -> h))))))"); }
	@Test public void testCond5() { testParseCond("   a=>b\n   c=>d\n   e=>f\n   h", "a.lazyIfTrue((() -> b), (() -> c.lazyIfTrue((() -> d), (() -> e.lazyIfTrue((() -> f), (() -> h))))))"); }
	@Test public void testCond6() { testParseCond("a < 0 => -a\n... => a", "a.cmp(0).less().lazyIfTrue((() -> a.negate()), (() -> a))"); }
	@Test public void testCond7() { testParseCond("a=>b\nc", "a.lazyIfTrue((() -> b), (() -> c))"); }
	
	@Test public void testLazyOr() { testParseCond("a || b", "a.lazyOr((() -> b))"); }
	@Test public void testLazyAnd() { testParseCond("a && b", "a.lazyAnd((() -> b))"); }
	@Test public void testLazyAndOr() { testParseCond("a && b || c", "a.lazyAnd((() -> b)).lazyOr((() -> c))"); }
	@Test public void testLazyOrAnd() { testParseCond("a || b && c", "a.lazyOr((() -> b.lazyAnd((() -> c))))"); }
	
	@Test public void testBadCond2() { testParseCond("a=>b\n  c", ExpectedOperator.class, 2); }

	@Test public void testSingleCond() { testParseCond("false => true", MissingElseClauseInConditional.class, 1); }
	
	public static void testParseCond(String source, String expectedSource) {
		ParseTestUtils.testParse(source, 0, null, Call.class, expectedSource);
	}
	private void testParseCond(String source, Class<? extends BanjoParseException> expectedError, int expectedErrorCount) {
		ParseTestUtils.testParse(source, expectedErrorCount, expectedError, BaseExpr.class, null);
	}
	
}
