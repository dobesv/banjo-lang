package banjo.parser.test;

import org.junit.Test;

import banjo.dom.core.CoreExpr;
import banjo.dom.core.ListLiteral;

public class TestListLiteralParser {

	//	@Test public void bullets() { list("\u2022 1\n\u2022 2\n\u2022 3", 0); }
	//	@Test public void stars() { list("* 1\n* 2\n* 3", 0); }
	@Test public void singletonList() { ParseTestUtils.test("[a]", "[a]", ListLiteral.class); }
	@Test public void bracketsNewlines() { list("[1\n 2\n 3]", 0); }
	@Test public void bracketsCommas() { list("[1,2,3]", 0); }
	@Test public void bracketsMixedNewlinesCommas() { list("[1\n 2,\n 3]", 0); }
	@Test public void invalidDedent() { list("[1,2,\n3]", 1); } // Expect an error since the 3 is at less indentation than the 1 and 2
	@Test public void bracketsTrailingComma() {  list("[1,2,3,]", 0); }
	//	@Test public void table1() { parse("#::a,b\n* 1,2\n* 3,4\n* 5,6", 0, "[{a: 1, b: 2}, {a: 3, b: 4}, {a: 5, b: 6}]"); }
	//	@Test public void table2() { parse("#::a,b\n(1,2)\n(3,4)\n(5,6)", 0, "[{a: 1, b: 2}, {a: 3, b: 4}, {a: 5, b: 6}]"); }

	public ListLiteral parse(String source, int expectedErrorCount, String expectedSource) {
		return ParseTestUtils.test(source, expectedErrorCount, null, ListLiteral.class, expectedSource);
	}

	private void list(String source, int expectedErrorCount) {
		final ListLiteral node = ParseTestUtils.test(source, expectedErrorCount, null, ListLiteral.class, "[1, 2, 3]");
		if(expectedErrorCount == 0) {
			final CoreExpr[] eltsArray = node.getElements().array(CoreExpr[].class);
			final long[] expectedLongValues = {1,2,3};
			for(int i=0; i < expectedLongValues.length; i++) {
				ParseTestUtils.assertIsNumberLiteralWithValue(expectedLongValues[i], eltsArray[i]);
			}
		}
	}
}
