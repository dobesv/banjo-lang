package banjo.parser.test;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import banjo.expr.core.Call;
import banjo.expr.core.ObjectLiteral;
import banjo.expr.core.Projection;
import fj.data.Stream;

@RunWith(Parameterized.class)
public class TestProjectionParser extends BaseParserTest {
	public TestProjectionParser(String source, String expectedToString, Class<?> expectedClass) {
		super(source, expectedToString, expectedClass);
    }

	@Parameters(name="{1}")
	public static Stream<Object[]> parameters() {
		return Stream.stream(
				test("a.b", Projection.class),
				test("a.b.c", Projection.class),
				test("a:b", Projection.class),
				test("a:b.c", Projection.class),
				test("a.{b}", "{b = a.b}", ObjectLiteral.class),
				test("a.{b,c}", "{b = a.b, c = a.c}", ObjectLiteral.class),
				test("a.{bb = b,c}", "{bb = a.b, c = a.c}", ObjectLiteral.class),
				test("a.{f(x) = g(x)}", "{f(x) = a.g(x)}", ObjectLiteral.class),
            test("{t = [x, y].map((z) -> (x)).min\n}", "{t = [x, y].map((z) ↦ x).min}", ObjectLiteral.class),
            test("{\n t = [\n x, \n y].length\n}", "{t = [x, y].length}", ObjectLiteral.class),
				test("a.\\-\\-", Projection.class),
				test("a.\\.\\.", Projection.class),
				test("a.\\.", Projection.class),
            test("a*.b", "(.b) ∘ a", Call.class)
		);
	}
}
