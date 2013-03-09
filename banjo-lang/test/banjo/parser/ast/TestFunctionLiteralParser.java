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
		final ParserReader in = ParserReader.fromString(getClass().getName(), source);
		final BanjoParser parser = new BanjoParser(in);
		final Expr node = parser.parseExpr();
		System.out.println(node.toSource());
		assertEquals(FunctionLiteral.class, node.getClass());
		FunctionLiteral func = (FunctionLiteral) node;
		assertNotNull(func);
		assertTrue(in.read() == -1);
		assertEquals(expectedErrors, parser.getErrors().size());
		assertEquals(expectedArgCount, func.getArgs().size());
		assertEquals("["+expectedArgNames+"]", func.getArgs().toString());
		assertEquals(IdRef.class, func.getBody().getClass());
		assertEquals(expectedArgReturned, ((IdRef)func.getBody()).getId());
		return func;
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
