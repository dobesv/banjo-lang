package banjo.dom.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import banjo.dom.core.CoreExpr;
import banjo.dom.core.SetLiteral;

public class TestSetLiteralParser {

	@Test public void bracketsNewlines() throws Exception { set("{1\n 2\n 3}", 0); }
	@Test public void bracketsCommas() throws Exception { set("{1,2,3}", 0); }
	@Test public void bracketsMixedNewlinesCommas() throws Exception { set("{1\n 2,\n 3}", 0); }
	@Test public void invalidDedent() throws Exception { set("{1,2,\n3}", 1); } // Expect an error since the 3 is at less indentation than the 1 and 2
	@Test public void bracketsTrailingComma() throws Exception {  set("{1,2,3,}", 0); }
	@Test public void pipes() throws Exception {  set("|1\n|2\n|3", 0); }
	@Test
	public void empty() {
		final SetLiteral node = ParseTestUtils.test("{}", SetLiteral.class);
		assertTrue(node.isEmpty());
	}

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
