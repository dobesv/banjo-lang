package banjo.dom.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Test;

import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.token.Identifier;


public class TestFunctionLiteralParser {

	public FunctionLiteral testParse(String source, int expectedErrors, int expectedArgCount, String expectedArgNames, String expectedArgReturned) {
		return testParse(source, expectedErrors, expectedArgCount, null, expectedArgNames, expectedArgReturned);
	}
	public FunctionLiteral testParse(String source, int expectedErrors, int expectedArgCount, String selfName, String expectedArgNames, final String expectedArgReturned) {
		final String normalizedSource =
				(selfName==null?"":selfName+".")+
				(expectedArgNames==null?"-> ":"("+expectedArgNames+") -> ")+expectedArgReturned;
		final FunctionLiteral func = ParseTestUtils.test(source, expectedErrors, null, FunctionLiteral.class, normalizedSource);
		func.getBody().acceptVisitor(new BaseCoreExprVisitor<Void>() {
			@Override
			@Nullable
			public Void fallback(@NonNull CoreExpr unsupported) {
				fail("Expected identifier: "+unsupported);
				return null;
			}

			@Override
			@Nullable
			public Void identifier(@NonNull Identifier n) {
				assertEquals(expectedArgReturned, n.getId());
				return null;
			}
		});
		return func;
	}
	@Test public void testIdentity()   { testParse("a↦a", 0, 1, "a", "a"); } // Identity function
	@Test public void testFst()        { testParse("a,b↦a", 0, 2, "a, b", "a"); } // First argument
	@Test public void testSnd()        { testParse("a,b↦b", 0, 2, "a, b", "b"); } // Second argument
	@Test public void testThird()      { testParse("a,b,c↦c", 0, 3, "a, b, c", "c"); } // Second argument
	@Test public void testLazyZ()      { testParse("↦z", 0, 0, null, "z"); } // Lazy value
	@Test public void testIdentity2()  { testParse("(a)↦a", 0, 1, "a", "a"); } // Identity function
	@Test public void testFst2()       { testParse("(a,b)↦a", 0, 2, "a, b", "a"); } // First argument
	@Test public void testSnd2()       { testParse("(a,b)↦b", 0, 2, "a, b", "b"); } // Second argument
	@Test public void testThird2()     { testParse("(a,b,c)↦c", 0, 3, "a, b, c", "c"); } // Second argument
	@Test public void testLazyParens() { testParse("()↦z", 0, 0, null, "z"); } // Lazy value
	@Test public void testSelfName()   { testParse("t.()->t", 0, 0, "t", "", "t"); }
	@Test public void testSelfName1()   { testParse("t.(a)->a", 0, 0, "t", "a", "a"); }
	@Test public void testSelfName2()   { testParse("x.(b,a)->b", 0, 0, "x", "b, a", "b"); }
}
