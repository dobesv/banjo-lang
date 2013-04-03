package banjo.parser.ast;

import org.junit.Test;

import banjo.parser.errors.BanjoParseException;
import banjo.parser.errors.ExpectedOperator;
import banjo.parser.errors.MissingElseClauseInConditional;

public class TestCondParser {

	@Test public void testCond1() { testParseCond("false => true; ... => false", "false.ifTrue((() -> true), (() -> false))()"); }
	@Test public void testCond2() { testParseCond("false => true\n... => false", "false.ifTrue((() -> true), (() -> false))()"); }
	@Test public void testCond3() { testParseCond("a => b\nc => d\nf", "a.ifTrue((() -> b), (() -> c.ifTrue((() -> d), (() -> f))()))()"); }
	@Test public void testCond4() { testParseCond("a=>b\nc=>d\ne=>f\nh", "a.ifTrue((() -> b), (() -> c.ifTrue((() -> d), (() -> e.ifTrue((() -> f), (() -> h))()))()))()"); }
	@Test public void testCond5() { testParseCond("   a=>b\n   c=>d\n   e=>f\n   h", "a.ifTrue((() -> b), (() -> c.ifTrue((() -> d), (() -> e.ifTrue((() -> f), (() -> h))()))()))()"); }
	@Test public void testCond6() { testParseCond("a < 0 => -a\n... => a", "a.cmp(0).less().ifTrue((() -> a.negate()), (() -> a))()"); }

	@Test public void testCond7() { testParseCond("a=>b\nc", "a.ifTrue((() -> b), (() -> c))()"); }
	
	@Test public void testBadCond2() { testParseCond("a=>b\n  c", ExpectedOperator.class, 2); }

	@Test public void testSingleCond() { testParseCond("false => true", MissingElseClauseInConditional.class, 1); }
	
	public static void testParseCond(String source, String expectedSource) {
		ParseTestUtils.testParse(source, 0, null, Call.class, expectedSource);
	}
	private void testParseCond(String source, Class<? extends BanjoParseException> expectedError, int expectedErrorCount) {
		ParseTestUtils.testParse(source, expectedErrorCount, expectedError, BaseExpr.class, null);
	}
	
}
