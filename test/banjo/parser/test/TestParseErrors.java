package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Test;

import banjo.dom.BadExpr;
import banjo.dom.source.BadSourceExpr.ExpectedOperator;
import banjo.dom.source.SourceErrorGatherer;
import banjo.dom.source.SourceExpr;
import banjo.parser.BanjoParser;
import banjo.parser.util.FileRange;
import banjo.parser.util.UnexpectedIOExceptionError;

public class TestParseErrors {

	@Test public void testMissingOperator1() { test("a b", ExpectedOperator.class, 1, 2); }

	private static void test(String source, Class<ExpectedOperator> expectedErrorClass, int errorStart, int errorEnd) {
		System.out.println("Source input:\n  "+source.replace("\n", "\n  "));
		SourceExpr parsed;
		try {
			parsed = new BanjoParser().parse(source);
		} catch (final IOException e1) {
			throw new UnexpectedIOExceptionError(e1);
		}
		int count = 0;
		for(final BadExpr e : SourceErrorGatherer.getProblems(parsed)) {
			final FileRange range = e.getSourceFileRange().getFileRange();
			System.out.println("  "+range+": "+e.getMessage());
			assertEquals(errorStart, range.getStartOffset());
			assertEquals(errorEnd, range.getEndOffset());
			count ++;
		}
		assert count == 1: "Expected a parse error";
	}
}
