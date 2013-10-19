package banjo.analysis.test;

import static banjo.parser.util.Check.nonNull;
import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.net.URI;

import org.eclipse.jdt.annotation.NonNull;
import org.junit.Test;

import banjo.analysis.DefRefAnalyser;
import banjo.analysis.DefRefAnalyser.Analysis;
import banjo.analysis.DefRefAnalyser.SourceRangeAnalysis;
import banjo.desugar.BanjoDesugarer;
import banjo.desugar.BanjoDesugarer.DesugarResult;
import banjo.dom.core.CoreExpr;
import banjo.parser.BanjoParser;
import banjo.parser.BanjoParser.ExtSourceExpr;
import banjo.parser.util.FileRange;
import banjo.parser.util.ReverseOrd;
import banjo.parser.util.UnexpectedIOExceptionError;
import fj.Ord;
import fj.P2;
import fj.data.TreeMap;

public class TestDefRefAnalyser {
	@NonNull
	private static final URI TEST_URI = nonNull(URI.create("test"));

	@Test public void free1() { test("a", "0:1F"); }
	@Test public void free2() { test("a + b", "0:1F,4:1F"); }
	@Test public void unused1() { test("a = 1; b = 2; a", "0:1D,7:1U,14:1R0"); }
	@Test public void unused2() { test("a = 1; b = 2; b", "0:1U,7:1D,14:1R7"); }
	@Test public void shadow1() { test("a = 1; a = 2; a", "0:1U,7:1S,14:1R7"); }
	@Test public void shadow2() { test("a = 1\na = 2\na", "0:1U,6:1S,12:1R6"); }

	@Test public void let1() { test("a = 1 ; a", "0:1D,8:1R0"); }
	@Test public void letf1() { test("f(x,y) = y+x+x ; f(10)", "0:1D,2:1D,4:1D,9:1R4,11:1R2,13:1R2,17:1R0"); }

	static void test(String src, String expectedCodes) {
		System.out.println(">> "+src);
		ExtSourceExpr parseResult;
		try {
			parseResult = new BanjoParser().parse(nonNull(src));
		} catch (final IOException e) {
			throw new UnexpectedIOExceptionError(e);
		}
		final DesugarResult<CoreExpr> dsResult = new BanjoDesugarer(parseResult.getSourceMap()).desugar(parseResult.getExpr());
		final DefRefAnalyser analyser = new DefRefAnalyser();
		final Analysis analysis = analyser.analyse(TEST_URI, dsResult.getValue(), parseResult.getFileRange());
		final SourceRangeAnalysis ranges = analysis.calculateSourceRanges(dsResult.getDesugarMap(), parseResult.getSourceMap());
		TreeMap<FileRange,String> rangeCodes = TreeMap.empty(ReverseOrd.reverseOrd(Ord.<FileRange>comparableOrd()));
		for(final FileRange r : ranges.getFree()) {
			System.out.println("F "+r);
			rangeCodes = rangeCodes.set(r, "F");
		}
		for(final P2<FileRange,FileRange> rp : ranges.getRefs()) {
			System.out.println("R "+rp._1()+" -> "+rp._2());
			rangeCodes = rangeCodes.set(rp._1(), "R"+rp._2().getStartOffset());
			rangeCodes = rangeCodes.set(rp._2(), "D");
		}
		for(final FileRange r : ranges.getUnusedDefs()) {
			System.out.println("U "+r);
			rangeCodes = rangeCodes.set(r, "U");
		}
		for(final FileRange r : ranges.getShadowingDefs()) {
			System.out.println("S "+r);
			rangeCodes = rangeCodes.set(r, "S");
		}

		final StringBuffer actualCodes = new StringBuffer();
		for(final P2<FileRange, String> cp : rangeCodes) {
			if(actualCodes.length() > 0) actualCodes.append(',');
			final FileRange range = cp._1();
			actualCodes.append(range.getStartOffset());
			actualCodes.append(':');
			actualCodes.append(range.length());
			actualCodes.append(cp._2());
		}
		assertEquals(expectedCodes, actualCodes.toString());
	}
}
