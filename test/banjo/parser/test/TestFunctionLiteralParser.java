package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import banjo.dom.core.CoreExpr;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.token.Key;


public class TestFunctionLiteralParser {

	public ObjectLiteral testParse(String source, int expectedErrors, int expectedArgCount, String expectedArgNames, String expectedArgReturned) {
		return testParse(source, expectedErrors, expectedArgCount, null, expectedArgNames, expectedArgReturned);
	}
	public ObjectLiteral testParse(String source, int expectedErrors, int expectedArgCount, String selfName, String expectedArgNames, final String expectedArgReturned) {
		final String normalizedSource =
				(selfName==null?"":selfName)+
				(expectedArgNames==null?"-> ":"("+expectedArgNames+") -> ")+expectedArgReturned;
		ParseTestUtils.test(source, expectedErrors, null, ObjectLiteral.class, normalizedSource);
		final ObjectLiteral func = (ObjectLiteral)CoreExpr.fromString(source);
		assert func.isLambda();
		if(expectedArgNames == null) assert func.isLazyValue();
		else {
			final StringBuffer sb = new StringBuffer();
			sb.append("(");
			for(Key arg : func.getMethods().head().getArgumentLists().head()) {
				if(sb.length() > 1) sb.append(", ");
				arg.toSource(sb, "");
			}
			sb.append(")");
			assertEquals(sb.toString(), "("+expectedArgNames+")");
		}
//		final CoreExpr body = func.getMethods().iterator().next().getBody();
//		body.acceptVisitor(new BaseCoreExprVisitor<Unit>() {
//			@Override
//			public Unit fallback() {
//				fail("Expected identifier: "+body);
//				return Unit.unit();
//			}
//
//			@Override
//			public Unit identifier(Identifier n) {
//				assertEquals(expectedArgReturned, n.getId());
//				return Unit.unit();
//			}
//		});
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

	@Test public void testUnpackObjectNoParens() { testParse("{a,b}↦a", 0, 1, null, "a", "((a, b) -> a)(a.a, a.b)"); }
	@Test public void testUnpackListNoParens() { testParse("[a,b]↦a", 0, 1, null, "a", "((a, b) -> a)(a.get(0), a.get(1))"); }
}
