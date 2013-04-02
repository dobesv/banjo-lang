package banjo.parser.ast;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class TestListLiteralParser {

	@Test public void bullets() throws Exception { list("\u2022 1\n\u2022 2\n\u2022 3", 0); }
	@Test public void stars() throws Exception { list("* 1\n* 2\n* 3", 0); }
	@Test public void bracketsNewlines() throws Exception { list("[1\n 2\n 3]", 0); }
	@Test public void bracketsCommas() throws Exception { list("[1,2,3]", 0); }
	@Test public void bracketsMixedNewlinesCommas() throws Exception { list("[1\n 2,\n 3]", 0); }
	@Test public void invalidDedent() throws Exception { list("[1,2,\n3]", 1); } // Expect an error since the 3 is at less indentation than the 1 and 2
	@Test public void bracketsTrailingComma() throws Exception {  list("[1,2,3,]", 0); }
	@Test public void table1() { parse("#::a,b\n* 1,2\n* 3,4\n* 5,6", 0, "[{a: 1, b: 2}, {a: 3, b: 4}, {a: 5, b: 6}]"); }
	@Test public void table2() { parse("#::a,b\n(1,2)\n(3,4)\n(5,6)", 0, "[{a: 1, b: 2}, {a: 3, b: 4}, {a: 5, b: 6}]"); }

	public ListLiteral parse(String source, int expectedErrorCount, String expectedSource) {
		return ParseTestUtils.testParse(source, expectedErrorCount, null, ListLiteral.class, expectedSource);
	}

	private void list(String source, int expectedErrorCount) {
		ListLiteral node = ParseTestUtils.testParse(source, expectedErrorCount, null, ListLiteral.class, null);
		final Object[] eltsArray = node.getElements().toArray();
		assertEquals(3, eltsArray.length);
		assertEquals(NumberLiteral.class, eltsArray[0].getClass());
		assertEquals(1L, ((NumberLiteral)eltsArray[0]).getNumber().longValue());
		assertEquals(NumberLiteral.class, eltsArray[1].getClass());
		assertEquals(2L, ((NumberLiteral)eltsArray[1]).getNumber().longValue());
		assertEquals(NumberLiteral.class, eltsArray[2].getClass());
		assertEquals(3L, ((NumberLiteral)eltsArray[2]).getNumber().longValue());
	}
}
