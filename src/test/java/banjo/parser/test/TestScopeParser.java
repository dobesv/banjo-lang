package banjo.parser.test;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import banjo.expr.core.Extend;
import banjo.expr.core.ScopedExpr;
import fj.data.Stream;

@RunWith(Parameterized.class)
public class TestScopeParser extends BaseParserTest {
    public TestScopeParser(String source, String expectedToString, Class<?> expectedClass) {
        super(source, expectedToString, expectedClass);
    }

    @Parameters(name = "{1}")
    public static Stream<Object[]> parameters() {
        return Stream.stream(
            test("a.b", ScopedExpr.class),
            test("a.b.c", ScopedExpr.class),
            test("a.{b}", "a.{b}", ScopedExpr.class),
            test("a.{b,c}", "a.{b, c}", ScopedExpr.class),
            test("a.{bb = b,c}", "a.{bb = b, c}", Extend.class),
            test("a.{f(x) = g(x)}", "a.{f(x) = g(x)}", Extend.class),
                test("{t = [x, y].map((z) -> (x)).min\n}", "{t = [x, y].map(z ↦ x).min}", Extend.class),
            test("{\n t = [\n   x, \n   y\n ].length\n}", "{t = [x, y].length}", Extend.class),
            test("a.\\-\\-", ScopedExpr.class),
            test("a.\\.\\.", ScopedExpr.class),
            test("a.\\.", ScopedExpr.class),
                test("a*.b", ".b ∘ a", ScopedExpr.class));
    }
}
