package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.stream.StreamSupport;

import org.junit.Assert;
import org.junit.Test;

import banjo.expr.BadExpr;
import banjo.expr.source.SourceErrorGatherer;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.SourceExprFactory;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

public class TestParenMatching {
    // TODO Make sure we're smart about paren mismatches and report the "right"
    // error somehow

    @Test
    public void test1() {
        final SourceExprFactory parser = new SourceExprFactory("test");
        String src = "{(}";
        testParens(parser, src, 1, 1, 2);
    }

    public void testParens(final SourceExprFactory parser, String src, int expectedErrorCount,
            int... expectedErrorOffsets) {
        System.out.println("Source input:\n  "+src.replace("\n", "\n  "));
        try {
        	final SourceExpr parseTree = parser.parse(src);
            List<BadExpr> problems = SourceErrorGatherer.getProblems(parseTree);
            if(problems.isNotEmpty()) {
                System.out.println("Errors:");
                problems.forEach(e -> System.out.println("    " + e));
            }
            assertEquals("Wrong number of errors", expectedErrorCount, problems.length());
            Set<SourceFileRange> ranges = Set
                    .join(SourceFileRange.ORD,
                            Set.set(SourceFileRange.SET_ORD, problems.map(p -> p.getSourceFileRanges())));
            int[] problemOffsets = StreamSupport.stream(ranges.spliterator(), false)
                    .mapToInt(r -> r.getFileRange().getStartOffset()).toArray();
            Assert.assertArrayEquals(expectedErrorOffsets, problemOffsets);
            
        } catch (final IOException e1) {
        	throw new UncheckedIOException(e1);
        }
    }
}
