package banjo.parser.ast;

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import org.junit.Test;

import banjo.parser.BanjoParser.BanjoParseException;
import banjo.parser.util.ParserReader;

public class TestStringLiteralParser {

	@Test
	public void testHelloWorld() throws IOException {
		testParser("\"Hello, world!\"", "Hello, world!", 0);
	}
	
	@Test
	public void testEscapes() throws IOException {
		testParser("\"\\\"\\t\\n\\r\"", "\"\t\n\r", 0);
	}

	private void testParser(String sourceString, String expectedParsedString,
			int expectedErrorCount) throws IOException {
		ParserReader in = ParserReader.fromString("test1", sourceString);
		Collection<BanjoParseException> errors = new ArrayList<>();
		StringLiteral node = StringLiteral.parseStringLiteral(in, errors);
		assertEquals(expectedErrorCount, errors.size());
		assertEquals(expectedParsedString, node.getString());
	}
	
	@Test
	public void testHexEscapes() throws IOException {
		testParser("\"\\x01\"", "\u0001", 0);
		testParser("\"\\x10\"", "\u0010", 0);
		testParser("\"\\xF0\"", "\u00F0", 0);
		testParser("\"\\xF\"", "", 1);
		testParser("\"\\x\"", "", 1);
		testParser("\"\\x00\"", "\u0000", 0);
		
		testParser("\"\\u0001\"", "\u0001", 0);
		testParser("\"\\u0010\"", "\u0010", 0);
		testParser("\"\\u00F0\"", "\u00F0", 0);
		testParser("\"\\u2022\"", "\u2022", 0);
		testParser("\"\\u00F\"", "", 1);
		testParser("\"\\u00\"", "", 1);
		testParser("\"\\u0000\"", "\u0000", 0);
	}
	
}
