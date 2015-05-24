package banjo.eval.coreexpr.test;

import org.junit.runners.Parameterized.Parameters;

import fj.data.Stream;

public class TestLabels extends BaseSourceExprTest {
	public TestLabels(String src) {
	    super(src);
    }

	@Parameters(name="{0}")
	public static Stream<Object[]> parameters() {
		return Stream.stream(
			"true.label == \"true\"",
			"false.label == \"true\"",
			"0 . label == \"0\"",
			"1 . label == \"1\"",
			"2 . label == \"2\"",
			"-1 . label == \"1\"",
			"(x -> x).label == \"<function from :line 1 col 1>\"",
			"({a=b} @ {label = \"bla\"}).label == \"bla\"",
			"({label = \"bla\"} @ {a=b}).label == \"bla\"",
			"[1,2].label = \"[1, 2]\"",
			"[1,2].first.label == \"[1]\"",
			"[1,2].last.label == \"[2]\""
		).map(s -> new Object[] {s});
	}
}
