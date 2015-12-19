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

	private static void test(String source, Class<? extends BadExpr> expectedErrorClass, int errorStart, int errorEnd) {
		System.out.println("Source input:\n  "+source.replace("\n", "\n  "));
		SourceExpr parsed = SourceExpr.fromString(source);
		final List<BadExpr> problems = SourceErrorGatherer.getProblems(parsed);
		for(final BadExpr e : problems) {
			final FileRange range = e.getSourceFileRanges().toStream().head().getFileRange();
			System.out.println("  "+range+": "+e.getMessage());
		}
		assertEquals(1, problems.length());
		for(final BadExpr e : problems) {
			final FileRange range = e.getSourceFileRanges().toStream().head().getFileRange();
			assertEquals(errorStart + " - " + errorEnd, range.getStartOffset() + " - " + range.getEndOffset());
		}
	}

	@Test public void testUnclosedStringLiteralInParen1() { test("bla = \"abc", BadSourceExpr.class, 6, 10); }
	@Test public void testUnclosedStringLiteralInParen2() { test("(\n bla = \"abc\n)", BadExpr.class, 9, 14); }
	@Test public void testUnclosedStringLiteralInParen3() { test("(\n bla = \"\n)\n", BadExpr.class, 9, 11); }

    @Test
    public void testDedentInParens1() {
        test("(\n  bla = foo(\n1)\n  )", BadSourceExpr.class, 20, 21);
    }


}
