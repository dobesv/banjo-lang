package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import banjo.expr.core.CoreExpr;
import banjo.expr.core.ObjectLiteral;
import banjo.expr.source.Operator;
import banjo.expr.token.Identifier;
import banjo.expr.util.ListUtil;


public class TestFunctionLiteralParser {

    public ObjectLiteral testParse(String source, int expectedErrors, int expectedArgCount, String expectedArgNames,
            String expectedArgReturned) {
		return testParse(source, expectedErrors, expectedArgCount, null, expectedArgNames, expectedArgReturned);
	}

    public ObjectLiteral testParse(String source, int expectedErrors, int expectedArgCount, String selfName,
            String expectedArgNames, final String expectedBody) {
        final String argList = expectedArgNames.contains(",") || selfName != null ? "(" + expectedArgNames + ")"
                : expectedArgNames;
		final String normalizedSource =
				(selfName==null?"":selfName)+
                        argList + " " + Operator.FUNCTION.getOp() + " " + expectedBody;
        ObjectLiteral func;
        ParseTestUtils.test(source, expectedErrors, null, ObjectLiteral.class, normalizedSource);
        func = (ObjectLiteral) CoreExpr.fromString(source);
		final StringBuffer sb = new StringBuffer();
        String formalArgNames = ListUtil.insertCommas(func.slots.head().args.drop(2).map(Identifier::getId));
        assertEquals(expectedArgNames, formalArgNames);
		return func;
	}

    @Test
    public void testLazyZ() {
        testParse("↦z", 0, 0, "", "z");
    } // Lazy value
	@Test public void testIdentity()   { testParse("a↦a", 0, 1, "a", "a"); } // Identity function
	@Test public void testIdentity2()  { testParse("(a)↦a", 0, 1, "a", "a"); } // Identity function, parens added

    @Test
    public void testFst() {
        testParse("(a,b)↦a", 0, 2, "a, b", "a");
    } // First argument

    @Test
    public void testSnd() {
        testParse("(a,b)↦b", 0, 2, "a, b", "b");
    } // Second argument

    @Test
    public void testThird() {
        testParse("(a,b,c)↦c", 0, 3, "a, b, c", "c");
    } // Second argument

    @Test
    public void testLazyParens() {
        testParse("()↦z", 0, 0, "", "z");
    } // Lazy value, with parens

    @Test
    public void testSelfName1() {
        testParse("s(a)↦a", 0, 0, "s", "a", "a");
    } // Unary function, with parens and "self name"
	@Test public void testSelfName2() { testParse("s()↦s", 0, 0, "s", "", "s"); } // Nullary function, with parens and "self name"

    @Test
    public void testUnpackObjectNoParens() {
        testParse("{a,b}↦a", 0, 1, null, "__0", "((a = __0.a, b = __0.b) ⇒ a)");
    }

	@Test public void testUnpackObjectNested() { testParse("({character, list, boolean={true, false}}) -> a", 0, 1, "__0", "((character = __0.character, list = __0.list, true = __0.boolean.true, false = __0.boolean.false) ⇒ a)"); }

}
