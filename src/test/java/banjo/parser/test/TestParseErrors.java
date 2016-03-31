package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import banjo.expr.BadExpr;
import banjo.expr.source.BadSourceExpr;
import banjo.expr.source.SourceErrorGatherer;
import banjo.expr.source.SourceExpr;
import banjo.expr.util.FileRange;
import fj.data.List;

public class TestParseErrors {

    private static void test(String source, Class<? extends BadExpr> expectedErrorClass, Integer... errorRanges) {
		System.out.println("Source input:\n  "+source.replace("\n", "\n  "));
		SourceExpr parsed = SourceExpr.fromString(source);
		final List<BadExpr> problems = SourceErrorGatherer.getProblems(parsed);
		for(final BadExpr e : problems) {
			final FileRange range = e.getSourceFileRanges().toStream().head().getFileRange();
			System.out.println("  "+range+": "+e.getMessage());
		}
        String actualProblemRanges = problems
            .map((BadExpr p) -> p.getSourceFileRanges().iterator().next().getFileRange())
            .map(range -> range.getStartOffset() + " - " + range.getEndOffset())
            .foldLeft(((a, b) -> a.isEmpty() ? b : a + ", " + b), "");
        StringBuffer expectedProblemRanges = new StringBuffer();
        for(int i = 0; i < errorRanges.length; i += 2) {
            if(i != 0)
                expectedProblemRanges.append(", ");
            expectedProblemRanges.append(errorRanges[i]);
            expectedProblemRanges.append(" - ");
            expectedProblemRanges.append(errorRanges[i + 1]);
        }
        assertEquals(expectedProblemRanges.toString(), actualProblemRanges);
	}

	@Test public void testUnclosedStringLiteralInParen1() { test("bla = \"abc", BadSourceExpr.class, 6, 10); }
	@Test public void testUnclosedStringLiteralInParen2() { test("(\n bla = \"abc\n)", BadExpr.class, 9, 14); }
	@Test public void testUnclosedStringLiteralInParen3() { test("(\n bla = \"\n)\n", BadExpr.class, 9, 11); }

    @Test
    public void testDedentInParens1() {
        test("(\n  bla = foo(\n1)\n)", BadSourceExpr.class, 18, 19);
    }

    @Test
    public void testDedentInParens2() {
        test("   (\nbla = foo(1)\n   )\n", BadSourceExpr.class, 3, 4, 5, 8);
    }


}
