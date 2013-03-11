package banjo.parser.ast;

import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;

import org.junit.Test;

import banjo.parser.BanjoParser;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.ParserReader;

public class TestObjectLiteralParser {


	@Test public void testNewlineSeparated() throws Exception { test123("{a:1\n b:2\n c:3}", 0); }
	@Test public void testCommaSeparator() throws Exception { test123("{a:1,b:2,c:3}", 0); }
	@Test public void testNoCurliesOrCommas() throws Exception { test123(" a: 1\n b : 2\n c :3", 0); }
	@Test public void testMixCommasNewlines() throws Exception { test123("{a:1\n b:2,\n c:3}", 0); }
	@Test public void testBackdentError() throws Exception { test123("{a:1,b:2,\nc:3}", 1); }
	
	@Test public void testTrailingComma() throws Exception { test123("{a:1,b:2,c:3,}", 0); }
	

	private void test123(String source, int expectedErrorCount) throws IOException, BanjoParseException {
		ObjectLiteral node = parse(source, expectedErrorCount);
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

	public ObjectLiteral parse(String source, int expectedErrorCount)
			throws IOException, BanjoParseException {
		final ParserReader in = ParserReader.fromString(getClass().getName(), source);
		final BanjoParser parser = new BanjoParser(in);
		final Expr parsed = parser.parseExpr();
		for(Exception e : parser.getErrors()) {
			System.out.println(e.toString());
		}
		System.out.println(parsed.toSource());
		ObjectLiteral node = (ObjectLiteral) parsed;
		assertNotNull(node);
		assertEquals(expectedErrorCount, parser.getErrors().size());
		assertEquals(0, in.remaining());
		return node;
	}
	
	@Test
	public void testEmpty() throws Exception {
		ObjectLiteral node = parse("{}", 0);
		assertTrue(node.getFields().isEmpty());
	}
}
