package banjo.dom.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Test;

import banjo.dom.FunctionLiteral;
import banjo.dom.Identifier;


public class TestFunctionLiteralParser {

	public FunctionLiteral testParse(String source, int expectedErrors, int expectedArgCount, String expectedArgNames, String expectedArgReturned) {
		return testParse(source, expectedErrors, expectedArgCount, null, expectedArgNames, expectedArgReturned);
	}
	public FunctionLiteral testParse(String source, int expectedErrors, int expectedArgCount, String selfName, String expectedArgNames, String expectedArgReturned) {
		FunctionLiteral func = ParseTestUtils.test(source, expectedErrors, null, FunctionLiteral.class, (selfName==null?"":selfName+".")+"("+expectedArgNames+") -> "+expectedArgReturned);
		assertEquals(Identifier.class, func.getBody().getClass());
		assertEquals(expectedArgReturned, ((Identifier)func.getBody()).getId());
		return func;
	}
	@Test public void testIdentity()   { testParse("a↦a", 0, 1, "a", "a"); } // Identity function
	@Test public void testFst()        { testParse("a,b↦a", 0, 2, "a, b", "a"); } // First argument
	@Test public void testSnd()        { testParse("a,b↦b", 0, 2, "a, b", "b"); } // Second argument
	@Test public void testThird()      { testParse("a,b,c↦c", 0, 3, "a, b, c", "c"); } // Second argument
	@Test public void testLazyZ()      { testParse("↦z", 0, 0, "", "z"); } // Lazy value
	@Test public void testIdentity2()  { testParse("(a)↦a", 0, 1, "a", "a"); } // Identity function
	@Test public void testFst2()       { testParse("(a,b)↦a", 0, 2, "a, b", "a"); } // First argument
	@Test public void testSnd2()       { testParse("(a,b)↦b", 0, 2, "a, b", "b"); } // Second argument
	@Test public void testThird2()     { testParse("(a,b,c)↦c", 0, 3, "a, b, c", "c"); } // Second argument
	@Test public void testLazyParens() { testParse("()↦z", 0, 0, "", "z"); } // Lazy value
	@Test public void testSelfName()   { testParse("t.()->t", 0, 0, "t", "", "t"); }
	@Test public void testSelfName1()   { testParse("t.(a)->a", 0, 0, "t", "a", "a"); }
	@Test public void testSelfName2()   { testParse("x.(b,a)->b", 0, 0, "x", "b, a", "b"); }
}
