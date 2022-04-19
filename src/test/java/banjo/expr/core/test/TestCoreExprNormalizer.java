package banjo.expr.core.test;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprNormalizer;

public class TestCoreExprNormalizer {

    @Test
    public void testNegate() {
        testNormalize("-(1)", "-1");
        testNormalize("-(-1)", "1");
        testNormalize("-(-(-2))", "2");
    }

    @Test
    public void testAddKernelNumbers() {
        testNormalize("INT32_SUM(1z, 1z)", "2zi");
        testNormalize("INT64_SUM(1z, 1z)", "2zL");
        testNormalize("FLOAT32_SUM(1z, 1z)", "2.0zf");
        testNormalize("FLOAT64_SUM(1z, 1z)", "2.0zd");
        testNormalize("DECIMAL_SUM(1z, 1z)", "2zD");
    }

    @Test
    public void testNumberLiteral1() {
        testNormalize("1", "PROJECT_ROOT.language kernel number(1.kernel number value)");
    }

    @Test
    public void testStringLiteral1() {
        testNormalize("\"aaa\"", "LANGUAGE_KERNEL_STRING_FACTORY(\"aaa\"k)");
    }

    @Test
    public void testStringLiteral2() {
        testNormalize("\"aaa\".at(1)", "LANGUAGE_KERNEL_STRING_FACTORY(\"a\"k)");
    }

    @Test
    public void testLet1() {
        testNormalize("(a = 1) => a", "PROJECT_ROOT.language kernel number(1.kernel number value)");
    }

    @Test
    public void testSlot1() {
        testNormalize("{a = 1}.a", "PROJECT_ROOT.language kernel number(1.kernel number value)");
    }

    @Test
    public void testInnerSlot() {
        testNormalize("{a = {b = 1}}.a.b", "PROJECT_ROOT.language kernel number(1.kernel number value)");
    }

    @Test
    public void testBaseSlot1() {
        testNormalize("({a = 1} @ {b = 2}).a", "PROJECT_ROOT.language kernel number(1.kernel number value)");
    }

    @Test
    public void testExtensionSlot1() {
        testNormalize("({a = 1} @ {b = 2}).b", "PROJECT_ROOT.language kernel number(2.kernel number value)");
    }

    @Test
    public void testRecursion1() {
        testNormalize("(f(x) = if(x) then (f(!x)) else (2)) => f(true)",
                "PROJECT_ROOT.language kernel number(2.kernel number value)");
    }

    /**
     * Test omega function; this unconditionally recursive function shouldn't
     * normalize at all
     */
    @Test
    public void testOmegaFunction() {
        testNormalize("(ω ↦ ω(ω))(ω ↦ ω(ω))", "((ω) ↦ ω(ω))((ω) ↦ ω(ω))");
    }

    /**
     * Test omega function; this unconditionally recursive slot shouldn't
     * normalize at all
     */
    @Test
    public void testOmegaSlot() {
        testNormalize("{ω.ω = ω.ω}.ω", "{ω.ω = ω.ω}.ω");
    }

    @Test
    public void testEventFold1() {
        testNormalize("f(REACTIVE_VALUE(a, b, x -> x))", "REACTIVE_VALUE(a, b, f)");
    }

    @Test
    public void testEventCountPlus1() {
        testNormalize(
                "REACTIVE_VALUE(0, ((a,b) -> a + 1), x -> x) + 1", "REACTIVE_VALUE(1, ((a,b) -> a + 1), .+(1))");
    }

    @Test
    public void testMultipleReactiveValues() {
        testNormalize(
                "REACTIVE_VALUE(0, ((a,b) -> a + 1), x -> x) + REACTIVE_VALUE(10, ((a,b) -> a - 1), x -> x)",
                "REACTIVE_VALUE({j=0, k=10}, ({j, k}, b) -> {j = j + 1, k = k - 1}, {j, k} -> j + k)");
    }

    @Test
    public void testBehaviour1() {
        testNormalize(
                "(.condition).filter(REACTIVE_VALUE([], (a,b) -> a + [b], x -> x).last :? 0",
                "REACTIVE_VALUE(0, (old value, new value) -> if(new value.condition) then(new value) else (old value))");
    }

    @Test
    public void testBehaviour2() {
        testNormalize("(.slot << (.condition).filter(REACTIVE_VALUE([], (a,b) -> a + [b]), x -> x).last) :? 0",
                "REACTIVE_VALUE(0, (old value, new value) -> if(new value.condition) then(new value.slot) else (old value))"
        );
    }

    @Test
    public void testEmptyList() {
        testNormalize("[]", "[]");
    }

    @Test
    public void testEmptyListFallback() {
        testNormalize("[] ?: 1", "1");
    }

    @Test
    public void testNonEmptyListFallback() {
        testNormalize("[1] ?: 2", "1");
    }

    private void testNormalize(String src, String expectedOutput) {
        testNormalize("{}", src, expectedOutput);
    }

    private void testNormalize(String projectRootSrc, String src, String expectedOutput) {
        CoreExpr projectRoot = CoreExpr.fromString(projectRootSrc);
        CoreExpr srcExpr = CoreExpr.fromString(src);
        CoreExprNormalizer normalizer = CoreExprNormalizer.forProject(projectRoot);
        CoreExpr normExpr = normalizer.apply(srcExpr);
        String actualOutput = normExpr.toSource();
        assertEquals("Expected " + srcExpr + " to normalize to " + expectedOutput + " but got " + actualOutput, expectedOutput,
                actualOutput);

    }
}
