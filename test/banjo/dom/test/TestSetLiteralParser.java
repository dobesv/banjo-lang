package banjo.dom.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import banjo.dom.core.SetLiteral;
import banjo.dom.token.NumberLiteral;

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
		final Object[] eltsArray = node.getElements().toArray();
		assertEquals(3, eltsArray.length);
		assertEquals(NumberLiteral.class, eltsArray[0].getClass());
		assertEquals(1L, ((NumberLiteral)eltsArray[0]).getNumber().longValue());
		assertEquals(NumberLiteral.class, eltsArray[1].getClass());
		assertEquals(2L, ((NumberLiteral)eltsArray[1]).getNumber().longValue());
		assertEquals(NumberLiteral.class, eltsArray[2].getClass());
		assertEquals(3L, ((NumberLiteral)eltsArray[2]).getNumber().longValue());
		return node;
	}
}
