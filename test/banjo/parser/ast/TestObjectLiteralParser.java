package banjo.parser.ast;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class TestObjectLiteralParser {


	@Test public void newlineSeparated()  { abc("{a:1\n b:2\n c:3}", 0); }
	@Test public void commaSeparator()    { abc("{a:1,b:2,c:3}", 0); }
	@Test public void noCurliesOrCommas() { abc(" a: 1\n b : 2\n c :3", 0); }
	@Test public void mixCommasNewlines() { abc("{a:1\n b:2,\n c:3}", 0); }
	@Test public void backdentError()     { abc("{a:1,b:2,\nc:3}", 1); }
	
	@Test public void trailingComma()     { abc("{a:1,b:2,c:3,}", 0); }

	@Test public void method() { parse("{f(x): x}", 0, "{f: (x) -> x}"); }

	private void abc(String source, int expectedErrorCount) {
		ObjectLiteral node = parse(source, expectedErrorCount, "{a: 1, b: 2, c: 3}");
		final Field[] eltsArray = node.getFields().values().toArray(new Field[3]);
		assertEquals(3, eltsArray.length);
		assertEquals(NumberLiteral.class, eltsArray[0].getValue().getClass());
		assertEquals(1L, ((NumberLiteral)eltsArray[0].getValue()).getNumber().longValue());
		assertEquals("a", eltsArray[0].getIdentifier());
		assertEquals(NumberLiteral.class, eltsArray[1].getValue().getClass());
		assertEquals(2L, ((NumberLiteral)eltsArray[1].getValue()).getNumber().longValue());
		assertEquals("b", eltsArray[1].getIdentifier());
		assertEquals(NumberLiteral.class, eltsArray[2].getValue().getClass());
		assertEquals(3L, ((NumberLiteral)eltsArray[2].getValue()).getNumber().longValue());
		assertEquals("c", eltsArray[2].getIdentifier());
	}

	public ObjectLiteral parse(String source, int expectedErrorCount, String expectedSource) {
		return ParseTestUtils.testParse(source, expectedErrorCount, ObjectLiteral.class, expectedSource);
	}
	
	@Test
	public void testEmpty() {
		ObjectLiteral node = parse("{}", 0, null);
		assertTrue(node.getFields().isEmpty());
	}
}
