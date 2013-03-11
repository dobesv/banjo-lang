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
		for(Exception e : parser.getErrors()) {
			System.out.println(e);
		}
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
	@Test public void testIdentity() throws Exception { testParse("a↦a", 0, 1, "a", "a"); } // Identity function
	@Test public void testFst() throws Exception { testParse("a,b↦a", 0, 2, "a, b", "a"); } // First argument
	@Test public void testSnd() throws Exception { testParse("a,b↦b", 0, 2, "a, b", "b"); } // Second argument
	@Test public void testThird() throws Exception { testParse("a,b,c↦c", 0, 3, "a, b, c", "c"); } // Second argument
	@Test public void testLazyZ() throws Exception { testParse("↦z", 0, 0, "", "z"); } // Lazy value
	@Test public void testIdentity2() throws Exception { testParse("(a)↦a", 0, 1, "a", "a"); } // Identity function
	@Test public void testFst2() throws Exception { testParse("(a,b)↦a", 0, 2, "a, b", "a"); } // First argument
	@Test public void testSnd2() throws Exception { testParse("(a,b)↦b", 0, 2, "a, b", "b"); } // Second argument
	@Test public void testThird2() throws Exception { testParse("(a,b,c)↦c", 0, 3, "a, b, c", "c"); } // Second argument
	@Test public void testLazyWithParens() throws Exception { testParse("()↦z", 0, 0, "", "z"); } // Lazy value
	
}
