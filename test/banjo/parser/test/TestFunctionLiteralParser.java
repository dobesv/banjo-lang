package banjo.parser.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Test;

import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.token.Identifier;


public class TestFunctionLiteralParser {

	public ObjectLiteral testParse(String source, int expectedErrors, int expectedArgCount, String expectedArgNames, String expectedArgReturned) {
		return testParse(source, expectedErrors, expectedArgCount, null, expectedArgNames, expectedArgReturned);
	}
	public ObjectLiteral testParse(String source, int expectedErrors, int expectedArgCount, String selfName, String expectedArgNames, final String expectedArgReturned) {
		final String normalizedSource =
				(selfName==null?"":selfName)+
				(expectedArgNames==null?"-> ":"("+expectedArgNames+") -> ")+expectedArgReturned;
		final ObjectLiteral func = ParseTestUtils.test(source, expectedErrors, null, ObjectLiteral.class, normalizedSource);
		assert func.isLambda();
		if(expectedArgNames == null) assert func.isLazyValue();
		else {
			final StringBuffer sb = new StringBuffer();
			func.getMethods().head().getParts().head().formalArgListToSource(sb);
			assertEquals(sb.toString(), "("+expectedArgNames+")");
		}
		func.getMethods().iterator().next().getBody().acceptVisitor(new BaseCoreExprVisitor<Void>() {
			@Override
			@Nullable
			public Void fallback() {
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
	@Test public void testLazyZ()      { testParse("↦z", 0, 0, null, "z"); } // Lazy value
	@Test public void testIdentity()   { testParse("a↦a", 0, 1, "a", "a"); } // Identity function
	@Test public void testIdentity2()  { testParse("(a)↦a", 0, 1, "a", "a"); } // Identity function, parens added
	@Test public void testFst()       { testParse("(a,b)↦a", 0, 2, "a, b", "a"); } // First argument
	@Test public void testSnd()       { testParse("(a,b)↦b", 0, 2, "a, b", "b"); } // Second argument
	@Test public void testThird()     { testParse("(a,b,c)↦c", 0, 3, "a, b, c", "c"); } // Second argument
	@Test public void testLazyParens() { testParse("()↦z", 0, 0, null, "z"); } // Lazy value, with parens
	@Test public void testSelfName1() { testParse("s(a)↦a", 0, 0, "s", "a", "a"); } // Lazy value, with parens
	@Test public void testSelfName2() { testParse("s()↦s", 0, 0, "s", "", "s"); } // Lazy value, with parens
}
