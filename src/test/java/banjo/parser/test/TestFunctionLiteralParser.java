package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import banjo.expr.core.CoreExpr;
import banjo.expr.core.BindingExpr;
import banjo.expr.source.Operator;
import banjo.expr.token.Identifier;
import banjo.expr.util.ListUtil;
import fj.data.List;
import static banjo.parser.test.ParseTestUtils.test;

public class TestFunctionLiteralParser {

    public BindingExpr testParse(
        String source,
        int expectedErrors,
        int expectedArgCount,
        String expectedArgNames,
        String expectedArgReturned
    )
    {
        return testParse(source, expectedErrors, expectedArgCount, null, expectedArgNames, expectedArgReturned);
    }

    public BindingExpr testParse(
        String source,
        int expectedErrors,
        int expectedArgCount,
        String selfName,
        String expectedArgNames,
        final String expectedBody
    )
    {
        final String argList = expectedArgNames.contains(",") || selfName != null ? "(" + expectedArgNames + ")"
            : expectedArgNames;
        final String normalizedSource = expectedArgCount == 0 && selfName == null
            ? Operator.NULLARY_FUNCTION_LITERAL.getOp() + expectedBody
            : (selfName == null ? "" : selfName) + argList + " " + Operator.FUNCTION_ARROW.getOp() + " " + expectedBody;
        BindingExpr func;
        ParseTestUtils.test(source, expectedErrors, null, BindingExpr.class, normalizedSource);
        func = (BindingExpr) CoreExpr.fromString(source);
        String formalArgNames = ListUtil
            .insertCommas(func.getSimpleArgsList().orSome(List.nil()).map(Identifier::getId));
        assertEquals(expectedArgNames, formalArgNames);
        return func;
    }

    @Test
    public void testLazyZ() {
        testParse("↦z", 0, 0, "", "z");
    } // Lazy value

    @Test
    public void testIdentity() {
        testParse("a↦a", 0, 1, "a", "a");
    } // Identity function

    @Test
    public void testIdentity2() {
        testParse("(a)↦a", 0, 1, "a", "a");
    } // Identity function, parens added

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
    public void testUnpackObjectNoParens() {
        test("{a,b}↦a", "{a, b} ↦ a");
    }

    @Test
    public void testUnpackObjectParens() {
        test("({a,b})↦a", "{a, b} ↦ a");
    }

    @Test
    public void testUnpackObjectNested() {
        test(
            "({character, list, boolean={true, false}}) -> a",
            "{" + Identifier.ARG_0
                + ".{character, list, +boolean.{true, false}} ==> "
                + Identifier.BETA_REDUCTION
                + " = a}"
        );
    }

}
