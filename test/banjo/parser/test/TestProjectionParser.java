package banjo.parser.test;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import banjo.expr.core.Call;
import banjo.expr.core.ObjectLiteral;
import banjo.expr.core.SlotReference;
import fj.data.Stream;

@RunWith(Parameterized.class)
public class TestProjectionParser extends BaseParserTest {
	public TestProjectionParser(String source, String expectedToString, Class<?> expectedClass) {
		super(source, expectedToString, expectedClass);
    }

	@Parameters(name="{0}")
	public static Stream<Object[]> parameters() {
		return Stream.stream(
				test("a.b", SlotReference.class),
				test("a.b.c", SlotReference.class),
				test("a:b", SlotReference.class),
				test("a:b.c", SlotReference.class),
				test("a.{b}", "{b = a.b}", ObjectLiteral.class),
				test("a.{b,c}", "{b = a.b, c = a.c}", ObjectLiteral.class),
				test("a.{bb = b,c}", "{bb = a.b, c = a.c}", ObjectLiteral.class),
				test("a.{f(x) = g(x)}", "{f(x) = a.g(x)}", ObjectLiteral.class),
				test("a.\\-\\-", SlotReference.class),
				test("a.\\.\\.", SlotReference.class),
				test("a.\\.", SlotReference.class),
				test("a*.b", "a *> (.b)", Call.class),
				test("{t = [x, y].map((z) ↦ (x)).min\n}", "{t = [x, y].map((z) ↦ x).min}", ObjectLiteral.class),
				test("{t = [\nx, \ny].length\n}", "{t = [x, y].length}", ObjectLiteral.class)
		);
	}
}
