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
import banjo.parser.util.UnexpectedIOExceptionError;
import fj.Ord;
import fj.P2;
import fj.data.Set;
import fj.data.TreeMap;

public class TestDefRefAnalyser {
	@NonNull
	private static final URI TEST_URI = nonNull(URI.create("test"));

	@Test public void free1() { test("a", "0:1F"); }
	@Test public void free2() { test("a + b", "0:1F,4:1F"); }
	@Test public void unused1() { test("a = 1; b = 2; a", "0:1D a,7:1U b,14:1R a@0"); }
	@Test public void unused2() { test("a = 1; b = 2; b", "0:1U a,7:1D b,14:1R b@7"); }
	@Test public void shadow1() { test("a = 1; a = 2; a", "0:1U a,7:1D a,7:1S a,14:1R a@7"); }
	@Test public void shadow2() { test("a = 1\na = 2\na", "0:1U a,6:1D a,6:1S a,12:1R a@6"); }

	@Test public void let1() { test("a = 1 ; a", "0:1D a,8:1R a@0"); }
	@Test public void letf1() { test("f(x,y) = y+x+x ; f(10)", "0:1D f,2:1D x,4:1D y,9:1R y@4,11:1R x@2,13:1R x@2,17:1R f@0"); }

	@Test public void obj1() { test("{ string.cons(_, tail) = { length = 1 + tail.length }, empty = { }}", "2:6U string,14:1U _,17:4D tail,40:4R tail@17"); }
	@Test public void obj2() { test("cons(_, tail) = { length = 1 + tail.length } ; empty = { }", "0:4U cons,5:1U _,8:4D tail,31:4R tail@8,47:5U empty"); }
	@Test public void unpack1() { test("a({f,g}) = f(g); a(1)",
			"0:1D a,0:21R __g_obj14@2,2:5D __g_obj14,3:1D f,5:1D g,11:1R f@3,13:1R g@5,17:1R a@0"); }


	static void test(String src, String expectedCodes) {
		System.out.println(">> "+src);
		ExtSourceExpr parseResult;
		try {
			parseResult = new BanjoParser().parse(nonNull(src));
		} catch (final IOException e) {
			throw new UnexpectedIOExceptionError(e);
		}
		final DesugarResult<CoreExpr> dsResult = new BanjoDesugarer(parseResult.getSourceMap()).desugar(parseResult.getExpr());
		System.out.println(parseResult.getSourceMap().toString());
		System.out.println("Desugar result:\n    "+dsResult.getValue());
		System.out.println(dsResult.getDesugarMap());
		final DefRefAnalyser analyser = new DefRefAnalyser();
		final Analysis analysis = analyser.analyse(TEST_URI, dsResult.getValue(), parseResult.getFileRange());
		final SourceRangeAnalysis ranges = analysis.calculateSourceRanges(dsResult.getDesugarMap(), parseResult.getSourceMap());
		TreeMap<FileRange,Set<String>> rangeCodes = TreeMap.empty(Ord.<FileRange>comparableOrd());
		for(final P2<FileRange, String> p : ranges.getFree()) {
			final FileRange r = p._1();
			final String id = p._2();
			System.out.println("F "+id+" "+r);
			rangeCodes = addRangeCode(rangeCodes, r, "F");
		}
		for(final P2<P2<FileRange, String>, FileRange> rp : ranges.getRefs()) {
			@NonNull @SuppressWarnings("null") final FileRange r = rp._1()._1();
			final String id = rp._1()._2();
			System.out.println("R "+id+" "+r+" -> "+rp._2());
			rangeCodes = addRangeCode(rangeCodes, r, "R "+id+"@"+rp._2().getStartOffset());
			rangeCodes = addRangeCode(rangeCodes, rp._2(), "D "+id);
		}
		for(@NonNull @SuppressWarnings("null") final P2<FileRange, String> p : ranges.getUnusedDefs()) {
			final FileRange r = p._1();
			final String id = p._2();
			System.out.println("U "+id+" "+r);
			rangeCodes = addRangeCode(rangeCodes, r, "U "+id);
		}
		for(@NonNull @SuppressWarnings("null") final P2<FileRange, String> p : ranges.getShadowingDefs()) {
			final FileRange r = p._1();
			final String id = p._2();
			System.out.println("S "+id+" "+r);
			rangeCodes = addRangeCode(rangeCodes, r, "S "+id);
		}

		final StringBuffer actualCodes = new StringBuffer();
		for(final P2<FileRange, Set<String>> cp : rangeCodes) {
			final FileRange range = cp._1();
			for(final String s : cp._2()) {
				if(actualCodes.length() > 0) actualCodes.append(',');
				actualCodes.append(range.getStartOffset());
				actualCodes.append(':');
				actualCodes.append(range.length());
				actualCodes.append(s);
			}
		}
		assertEquals(expectedCodes, actualCodes.toString());
	}
	public static TreeMap<FileRange, Set<String>> addRangeCode(
			TreeMap<FileRange, Set<String>> rangeCodes, final FileRange r,
			final String s) {
		rangeCodes = rangeCodes.set(r, rangeCodes.get(r).orSome(Set.empty(Ord.stringOrd)).insert(s));
		return rangeCodes;
	}
}
