package banjo.parser.ast;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import org.junit.Test;

import banjo.parser.BanjoParser;
import banjo.parser.BanjoParser.BanjoParseException;
import banjo.parser.util.ParserReader;

public class TestObjectLiteralParser {


	@Test
	public void test123() throws Exception {
		test123("{a:1\n b:2\n c:3}", 0);
		test123("{a:1,b:2,c:3}", 0);
		test123("{a:1,b:2,c:3,}", 0);
		test123(" a: 1\n b : 2\n c :3", 0);
		test123("{a:1\n b:2,\n c:3}", 0);
		test123("{a:1,b:2,\nc:3}", 1);
		
	}

	private void test123(String source, int expectedErrorCount) throws IOException, BanjoParseException {
		Collection<BanjoParseException> errors = new ArrayList<>();
		final ParserReader in = ParserReader.fromString(getClass().getName(), source);
		ObjectLiteral node = BanjoParser.parseObjectLiteral(in, errors);
		assertNotNull(node);
		assertEquals(expectedErrorCount, errors.size());
		assertEquals(0, in.remaining());
		final Field[] eltsArray = node.getFields().values().toArray(new Field[3]);
		assertEquals(3, eltsArray.length);
		assertEquals(NumberLiteral.class, eltsArray[0].getValue().getClass());
		assertEquals(1L, ((NumberLiteral)eltsArray[0].getValue()).getNumber().longValue());
		assertEquals("a", eltsArray[0].getIdentifier().getText());
		assertEquals(NumberLiteral.class, eltsArray[1].getValue().getClass());
		assertEquals(2L, ((NumberLiteral)eltsArray[1].getValue()).getNumber().longValue());
		assertEquals("b", eltsArray[1].getIdentifier().getText());
		assertEquals(NumberLiteral.class, eltsArray[2].getValue().getClass());
		assertEquals(3L, ((NumberLiteral)eltsArray[2].getValue()).getNumber().longValue());
		assertEquals("c", eltsArray[2].getIdentifier().getText());
	}
}
