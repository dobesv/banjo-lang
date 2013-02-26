package banjo.parser.ast;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;

import org.junit.Test;

import banjo.parser.BanjoParser;
import banjo.parser.BanjoParser.BanjoParseException;
import banjo.parser.util.ParserReader;

public class TestListLiteralParser {

	@Test
	public void test123() throws Exception {
		test123("[1\n 2\n 3]", 0);
		test123("[1,2,3]", 0);
		test123("[1,2,3,]", 0);
		test123("\u2022 1\n\u2022 2\n\u2022 3", 0);
		test123("[1\n 2,\n 3]", 0);
		test123("[1,2,\n3]", 1);
		
	}

	private void test123(String source, int expectedErrorCount) throws IOException, BanjoParseException {
		final ParserReader in = ParserReader.fromString(getClass().getName(), source);
		final BanjoParser parser = new BanjoParser(in);
		ListLiteral node = parser.parseListLiteral();
		assertEquals(expectedErrorCount, parser.getErrors().size());
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
