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
import banjo.analysis.DefRefAnalyser.VarDefWithRange;
import banjo.analysis.DefRefAnalyser.VarRefWithRange;
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
	@Test public void obj3() { test("{self.a(b,c) = c.foo({ (d) = b + self }) }", "1:4D self,8:1D b,10:1D c,15:1R c@10,24:1U d,29:1R b@8,33:4R self@1"); }

	@Test public void obj4() { test("{(x + y) = ( x.a(y) => x ; x.b(x) => w )}", "2:1D x,6:1D y,17:1R y@6,37:1F w"); }

	@Test public void unpack1() { test("a({f,g}) = f(g); a(1)",
			"0:1D a,0:21R __g_obj14@2,2:5D __g_obj14,3:1D f,5:1D g,11:1R f@3,13:1R g@5,17:1R a@0"); }

	@Test public void optionalCall() { test("a.?b(c)", ""); }

	@Test public void xxx() { test("{\nnum.negative(abs) =\n"+
			"  abs.isZero => abs ; abs.isNegative => - abs ; {\n"+
			"    minusOne = num.negative(abs.plusOne)\n"+
			"    plusOne = num.negative(abs.minusOne) \n"+
			"    (- _) = abs\n"+
			"    (n + x) = (x - abs)\n"+
			"  }\n"+
			"}", ""); }

	static void test(String src, String expectedCodes) {
		System.out.println(">> "+src.replace("\n", "\n>> "));
		ExtSourceExpr parseResult;
		try {
			parseResult = new BanjoParser().parse(nonNull(src));
		} catch (final IOException e) {
			throw new UnexpectedIOExceptionError(e);
		}
		final DesugarResult<CoreExpr> dsResult = new BanjoDesugarer().desugar(parseResult.getExpr());
		System.out.println(parseResult.getSourceMap().toString());
		System.out.println("Desugar result:\n    "+dsResult.getValue());
		System.out.println(dsResult.getDesugarMap());
		final DefRefAnalyser analyser = new DefRefAnalyser();
		final Analysis analysis = analyser.analyse(TEST_URI, dsResult.getValue(), parseResult.getFileRange());
		final SourceRangeAnalysis ranges = analysis.calculateSourceRanges(dsResult.getDesugarMap(), parseResult.getSourceMap());
		TreeMap<FileRange,Set<String>> rangeCodes = TreeMap.empty(Ord.<FileRange>comparableOrd());
		for(final VarRefWithRange p : ranges.getFree()) {
			final FileRange r = p.getRange();
			final String id = p.getName();
			System.out.println("F "+id+" "+r);
			rangeCodes = addRangeCode(rangeCodes, r, "F "+id);
		}
		for(final P2<VarRefWithRange, FileRange> rp : ranges.getRefs()) {
			final FileRange r = rp._1().getRange();
			String id = rp._1().getName();
			final FileRange refRange = rp._2();
			final String refText = src.substring(refRange.getStartOffset(), refRange.getEndOffset());
			if(!refText.equals(id)) id = id +" ("+refText+")";
			System.out.println("R "+id+" "+r+" -> "+refRange);
			rangeCodes = addRangeCode(rangeCodes, r, "R "+id+"@"+refRange.getStartOffset());
			rangeCodes = addRangeCode(rangeCodes, refRange, "D "+id);
		}
		for(@NonNull @SuppressWarnings("null") final VarDefWithRange p : ranges.getUnusedDefs()) {
			final FileRange r = p.getRange();
			final String id = p.getName();
			System.out.println("U "+id+" "+r);
			rangeCodes = addRangeCode(rangeCodes, r, "U "+id);
		}
		for(@NonNull @SuppressWarnings("null") final VarDefWithRange p : ranges.getShadowingDefs()) {
			final FileRange r = p.getRange();
			final String id = p.getName();
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
