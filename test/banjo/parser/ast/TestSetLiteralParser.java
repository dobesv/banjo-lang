package banjo.parser.ast;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;

import org.junit.Test;

import banjo.parser.BanjoParser;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.ParserReader;

public class TestSetLiteralParser {

	@Test public void bracketsNewlines() throws Exception { set("{1\n 2\n 3}", 0); }
	@Test public void bracketsCommas() throws Exception { set("{1,2,3}", 0); }
	@Test public void bracketsMixedNewlinesCommas() throws Exception { set("{1\n 2,\n 3}", 0); }
	@Test public void invalidDedent() throws Exception { set("{1,2,\n3}", 1); } // Expect an error since the 3 is at less indentation than the 1 and 2
	@Test public void bracketsTrailingComma() throws Exception {  set("{1,2,3,}", 0); }

	private void set(String source, int expectedErrorCount) throws IOException, BanjoParseException {
		final ParserReader in = ParserReader.fromString(getClass().getName(), source);
		final BanjoParser parser = new BanjoParser(in);
		final Expr parsed = parser.parseExpr();
		for(Exception e : parser.getErrors()) {
			System.out.println(e.toString());
		}
		assertEquals(expectedErrorCount, parser.getErrors().size());
		System.out.println(parsed.toSource());
		assertEquals(SetLiteral.class, parsed.getClass());
		SetLiteral node = (SetLiteral) parsed;
		assertEquals(-1, in.read());
		assertNotNull(node);
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
