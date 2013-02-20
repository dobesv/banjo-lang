package banjo.parser.ast;

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import org.junit.Test;

import banjo.parser.BanjoParser;
import banjo.parser.BanjoParser.BanjoParseException;
import banjo.parser.util.ParserReader;

public class TestLetParser {
	@Test
	public void simpleTests() throws Exception {
		simpleHelloTest("hello = \"world\" ; hello", 0);
		simpleHelloTest("hello = \"world\"\nhello", 0);
		simpleHelloTest("   hello = \"world\"\n   hello", 0);
		simpleHelloTest("   hello = \"world\"\nhello", 1); // Backdent here should be reported as an error
		simpleHelloTest(" hello = \"world\"\n   hello", 1); // Indent here should be reported as an error
	}

	private void simpleHelloTest(String sourceString, int expectedErrorCount) throws IOException,
			BanjoParseException {
		ParserReader in = ParserReader.fromString("<test>", sourceString);
		Collection<BanjoParseException> errors = new ArrayList<>();
		Let node = BanjoParser.parseLet(in, errors);
		assertEquals(expectedErrorCount, errors.size());
		assertEquals("hello", node.getName());
		assertEquals(StringLiteral.class, node.getValue().getClass());
		assertEquals("world", ((StringLiteral)node.getValue()).getString());
		assertEquals(IdRef.class, node.getBody().getClass());
		assertEquals("hello", ((IdRef)node.getBody()).getId());
	}
}
