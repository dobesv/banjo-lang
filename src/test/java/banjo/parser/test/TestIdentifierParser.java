package banjo.parser.test;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import banjo.expr.token.Identifier;
import fj.data.Stream;

@RunWith(Parameterized.class)
public class TestIdentifierParser extends BaseParserTest {

	public TestIdentifierParser(String source, String expectedToString,
            Class<?> expectedClass) {
	    super(source, expectedToString, expectedClass);
    }

	@Parameters(name="{0}")
	public static Stream<Object[]> parameters() {
		return Stream.stream(
				test("abc", Identifier.class),
				test("ABC", Identifier.class),
				test("\\a\\b\\c", "abc", Identifier.class),
				test("\\.", Identifier.class),
				test("\\-\\ \\-", "\\-\\ \\-", Identifier.class),
				test("\\-\\-", Identifier.class)
		);
	}
}
