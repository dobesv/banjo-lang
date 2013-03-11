package banjo.parser.ast;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

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
		simpleHelloTest("   hello = \"world\"\nhello", 2); // Backdent here should be reported as an error
		simpleHelloTest(" hello = \"world\"\n   hello", 1); // Indent here should be reported as an error
	}

	private void simpleHelloTest(String sourceString, int expectedErrorCount) throws IOException,
			BanjoParseException {
		ParserReader in = ParserReader.fromString(getClass().getName(), sourceString);
		final BanjoParser parser = new BanjoParser(in);
		final Expr parsed = parser.parseExpr();
		System.out.println(parsed.toSource());
		for(Exception e : parser.getErrors()) {
			System.out.println(e.toString());
		}
		assertEquals(expectedErrorCount, parser.getErrors().size());
		assertEquals(Steps.class, parsed.getClass());
		Steps node = (Steps) parsed;
		assertEquals(2, node.getSteps().size());
		Let let = (Let) node.getSteps().get(0);
		
		assertEquals("hello", let.getName());
		assertEquals(StringLiteral.class, let.getValue().getClass());
		assertEquals("world", ((StringLiteral)let.getValue()).getString());
		Expr body = node.getSteps().get(1);
		assertEquals(IdRef.class, body.getClass());
		assertEquals("hello", ((IdRef)body).getId());
	}
}
