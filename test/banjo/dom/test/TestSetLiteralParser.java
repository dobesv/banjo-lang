package banjo.dom.test;

import static banjo.dom.test.ParseTestUtils.test;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import banjo.dom.core.CoreExpr;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.core.SetLiteral;

public class TestSetLiteralParser {

	@Test public void bracketsNewlines() throws Exception { set("{1\n 2\n 3}", 0); }
	@Test public void bracketsCommas() throws Exception { set("{1,2,3}", 0); }
	@Test public void bracketsMixedNewlinesCommas() throws Exception { set("{1\n 2,\n 3}", 0); }
	@Test public void invalidDedent() throws Exception { set("{1,2,\n3}", 1); } // Expect an error since the 3 is at less indentation than the 1 and 2
	@Test public void bracketsTrailingComma() throws Exception {  set("{1,2,3,}", 0); }
	@Test public void pipes() throws Exception {  set("|1\n|2\n|3", 0); }

	@Test public void setOfSets1() { test("{{1},{2},{3,4,5},{}}", "{{1}, {2}, {3, 4, 5}, {}}", SetLiteral.class); }

	// Empty set is actually the empty object ... a bit confusing.  Sharing {} between sets and objects is a mistake I think
	@Test public void empty() { ParseTestUtils.test("{}", ObjectLiteral.class); }

	private SetLiteral set(String source, int expectedErrorCount) {
		final SetLiteral node = ParseTestUtils.test(source, expectedErrorCount, null, SetLiteral.class, null);
		final CoreExpr[] eltsArray = node.getElements().toArray(new CoreExpr[0]);
		final long[] expectedLongValues = {1,2,3};
		assertEquals(expectedLongValues.length, eltsArray.length);
		for(int i=0; i < expectedLongValues.length; i++) {
			ParseTestUtils.assertIsNumberLiteralWithValue(expectedLongValues[i], eltsArray[i]);
		}
		return node;
	}
}
