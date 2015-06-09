package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import banjo.expr.BadExpr;
import banjo.expr.source.BadSourceExpr;
import banjo.expr.source.SourceErrorGatherer;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.BadSourceExpr.ExpectedOperator;
import banjo.expr.util.FileRange;

public class TestParseErrors {

	private static void test(String source, Class<? extends BadExpr> expectedErrorClass, int errorStart, int errorEnd) {
		System.out.println("Source input:\n  "+source.replace("\n", "\n  "));
		SourceExpr parsed = SourceExpr.fromString(source);
		int count = 0;
		for(final BadExpr e : SourceErrorGatherer.getProblems(parsed)) {
			final FileRange range = e.getSourceFileRanges().head().getFileRange();
			System.out.println("  "+range+": "+e.getMessage());
		}
		for(final BadExpr e : SourceErrorGatherer.getProblems(parsed)) {
			final FileRange range = e.getSourceFileRanges().head().getFileRange();
			assertEquals(errorStart + " - " + errorEnd, range.getStartOffset() + " - " + range.getEndOffset());
			count ++;
		}
		assert count == 1: "Expected a parse error";
	}

	@Test public void testUnclosedStringLiteralInParen1() { test("bla = \"abc", BadSourceExpr.class, 6, 10); }
	@Test public void testUnclosedStringLiteralInParen2() { test("(\n bla = \"abc\n)", BadExpr.class, 9, 14); }
	@Test public void testUnclosedStringLiteralInParen3() { test("(\n bla = \"\nabc\n)", BadExpr.class, 9, 11); }
	@Test public void testUnclosedStringLiteralInParen4() { test("(\n bla = \"\n  abc\n)", BadExpr.class, 9, 17); }


}
