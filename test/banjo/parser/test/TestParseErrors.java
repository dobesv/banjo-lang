package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import banjo.dom.BadExpr;
import banjo.dom.source.BadSourceExpr.ExpectedOperator;
import banjo.dom.source.SourceErrorGatherer;
import banjo.dom.source.SourceExpr;
import banjo.parser.util.FileRange;

public class TestParseErrors {

	@Test public void testMissingOperator1() { test("a b", ExpectedOperator.class, 1, 2); }

	private static void test(String source, Class<ExpectedOperator> expectedErrorClass, int errorStart, int errorEnd) {
		System.out.println("Source input:\n  "+source.replace("\n", "\n  "));
		SourceExpr parsed = SourceExpr.fromString(source);
		int count = 0;
		for(final BadExpr e : SourceErrorGatherer.getProblems(parsed)) {
			final FileRange range = e.getSourceFileRanges().head().getFileRange();
			System.out.println("  "+range+": "+e.getMessage());
			assertEquals(errorStart, range.getStartOffset());
			assertEquals(errorEnd, range.getEndOffset());
			count ++;
		}
		assert count == 1: "Expected a parse error";
	}
}
