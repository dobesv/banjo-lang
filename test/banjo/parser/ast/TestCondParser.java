package banjo.parser.ast;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class TestCondParser {

	@Test public void testCond1() { testParseCond("false => true; true => false", 2, 0, "false => true; true => false"); }
	@Test public void testCond2() { testParseCond("false => true\ntrue => false", 2, 0, "false => true; true => false"); }
	@Test public void testCond3() { testParseCond("a => b\nc => d\ne => f", 3, 0, "a => b; c => d; e => f"); }
	@Test public void testCond4() { testParseCond("a=>b\nc=>d\ne=>f\ng=>h", 4, 0, "a => b; c => d; e => f; g => h"); }
	@Test public void testCond5() { testParseCond("   a=>b\n   c=>d\n   e=>f\n   g=>h", 4, 0, "a => b; c => d; e => f; g => h"); }
	@Test public void testCond6() { testParseCond("a < 0 => -a\n... => a", 2, 0, "a < 0 => -a; ... => a"); }

	@Test public void testBadCond1() { testParseCond("a=>b\nc", 1); }
	@Test public void testBadCond2() { testParseCond("a=>b\n  c", 1); }

	@Test public void testSingleCond() { testParseCond("false => true", 1, 0, "false => true"); }
	
	public static void testParseCond(String source, int expectedCases,
			int expectedErrors, String expectedSource) {
		Cond node = ParseTestUtils.testParse(source, expectedErrors, Cond.class, expectedSource);
		assertEquals(expectedCases, node.getCases().size());
	}
	private void testParseCond(String source, int expectedErrors) {
		ParseTestUtils.testParse(source, expectedErrors, BaseExpr.class, null);
	}
	
}
