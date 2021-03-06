package banjo.parser.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.stream.StreamSupport;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import banjo.expr.BadExpr;
import banjo.expr.source.SourceErrorGatherer;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.SourceExprFactory;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;
import fj.data.Stream;

@RunWith(Parameterized.class)
public class TestParenMatching {
    public static Object[] t(String src, int expectedErrorCount, int... expectedErrorOffsets) {
        return new Object[] {
            src.replace("\n", " "), src, expectedErrorCount, expectedErrorOffsets
        };
    }

    @Parameters(name = "{0}")
    public static Stream<Object[]> parameters() {
        return Stream.stream(
            t("{(}", 1, 1, 2), 
            t("({)", 1, 1, 2), 
            t("[(]", 1, 1, 2), 
            t(")", 1, 0), 
            t("(", 1, 0),
            t("[[))", 1, 3),
            t("{(})", 1, 3),
            t("[]", 0),
            t("({})", 0),
            t("(\n{\n}\n)\n", 0),
            t("[\n{\n]\n", 1, 2, 4),
            t("[\n  [\n]\n", 1, 4),
            t("{\n  }}\n}\n", 1, 5)
            );
    }

    private String src;
    private int expectedErrorCount;
    private int[] expectedErrorOffsets;

    public TestParenMatching(String label, String src, int expectedErrorCount, int... expectedErrorOffsets) {
        this.src = src;
        this.expectedErrorCount = expectedErrorCount;
        this.expectedErrorOffsets = expectedErrorOffsets;
    }
    @Test
    public void testParens() {
        final SourceExprFactory parser = new SourceExprFactory("test");
        StringBuffer debugInfo = new StringBuffer();
        debugInfo.append("Source input:\n  " + src.replace("\n", "\n  "));
        try {
        	final SourceExpr parseTree = parser.parse(src);
            List<BadExpr> problems = SourceErrorGatherer.getProblems(parseTree);
            if(problems.isNotEmpty()) {
                debugInfo.append("\nErrors:");
                problems.forEach(e -> debugInfo.append("\n    " + e));
            }
            assertEquals("Wrong number of errors", expectedErrorCount, problems.length());
            Set<SourceFileRange> ranges = Set
                    .join(SourceFileRange.ORD,
                            Set.iterableSet(SourceFileRange.SET_ORD, problems.map(p -> p.getRanges())));
            int[] problemOffsets = StreamSupport.stream(ranges.spliterator(), false)
                    .mapToInt(r -> r.getFileRange().getStartOffset()).toArray();
            Assert.assertArrayEquals(expectedErrorOffsets, problemOffsets);
            
        } catch (final IOException e1) {
        	throw new UncheckedIOException(e1);
        } catch(AssertionError err) {
            throw new AssertionError(debugInfo.toString(), err);
        }
    }
}
