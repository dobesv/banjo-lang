package banjo.parser.ast;

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import org.junit.Test;

import banjo.parser.BanjoParser;
import banjo.parser.BanjoParser.BanjoParseException;
import banjo.parser.util.ParserReader;

public class TestFunctionLiteralParser {

	public FunctionLiteral testParse(String source, int expectedErrors, int expectedArgCount, String expectedArgNames, String expectedArgReturned) throws BanjoParseException, IOException {
		Collection<BanjoParseException> errors = new ArrayList<>();
		final ParserReader in = ParserReader.fromString(getClass().getName(), source);
		FunctionLiteral node = BanjoParser.parseFunctionLiteral(in, errors);
		assertNotNull(node);
		assertEquals(expectedErrors, errors.size());
		assertEquals(expectedArgCount, node.getArgs().size());
		assertEquals("["+expectedArgNames+"]", node.getArgs().keySet().toString());
		assertEquals(IdRef.class, node.getBody().getClass());
		assertEquals(expectedArgReturned, ((IdRef)node.getBody()).getId());
		return node;
	}
	@Test
	public void test1() throws Exception {
		testParse("a↦a", 0, 1, "a", "a"); // Identity function
		testParse("a,b↦a", 0, 2, "a, b", "a"); // First argument
		testParse("a,b↦b", 0, 2, "a, b", "b"); // Second argument
		testParse("a,b,c↦c", 0, 3, "a, b, c", "c"); // Second argument
		testParse("↦z", 0, 0, "", "z"); // Lazy value
		testParse("(a)↦a", 0, 1, "a", "a"); // Identity function
		testParse("(a,b)↦a", 0, 2, "a, b", "a"); // First argument
		testParse("(a,b)↦b", 0, 2, "a, b", "b"); // Second argument
		testParse("(a,b,c)↦c", 0, 3, "a, b, c", "c"); // Second argument
		testParse("()↦z", 0, 0, "", "z"); // Lazy value
	}
}
