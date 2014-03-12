package banjo.analysis.test;

import static banjo.parser.util.Check.nonNull;
import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.net.URI;

import org.eclipse.jdt.annotation.NonNull;
import org.junit.Test;

import banjo.analysis.DefRefAnalyser;
import banjo.analysis.DefRefAnalyser.Analysis;
import banjo.desugar.BanjoDesugarer;
import banjo.desugar.BanjoDesugarer.DesugarResult;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.MethodParamDecl;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.Key;
import banjo.parser.BanjoParser;
import banjo.parser.util.FileRange;
import banjo.parser.util.SourceFileRange;
import banjo.parser.util.UnexpectedIOExceptionError;
import fj.Ord;
import fj.P2;
import fj.data.Set;
import fj.data.TreeMap;

public class TestDefRefAnalyser {
	@NonNull
	private static final URI TEST_URI = nonNull(URI.create("test"));

	@Test public void free1() { test("a", "0:1F a"); }
	@Test public void free2() { test("a + b", "0:1F a,4:1F b"); }
	@Test public void unused1() { test("a = 1; b = 2; a", "0:1D a,7:1U b,14:1R a@0"); }
	@Test public void unused2() { test("a = 1; b = 2; b", "0:1U a,7:1D b,14:1R b@7"); }
	@Test public void shadow1() { test("a = 1; a = 2; a", "0:1U a,7:1D a,7:1S a,14:1R a@7"); }
	@Test public void shadow2() { test("a = 1\na = 2\na", "0:1U a,6:1D a,6:1S a,12:1R a@6"); }

	@Test public void let1() { test("a = 1 ; a", "0:1D a,8:1R a@0"); }
	@Test public void letf1() { test("f(x,y) = y+x+x ; f(10)", "0:1D f,0:1SU f,2:1D x,4:1D y,9:1R y@4,11:1R x@2,13:1R x@2,17:1R f@0"); }
	@Test public void letf2() { test("f(x) = x.f(x) ; f(10)", "0:1D f,0:1SU f,2:1D x,8:1R x@2,11:1R x@2,16:1R f@0"); }
	@Test public void letf3() { test("f(x) = x.f(x,x,x) ; f(10)", "0:1D f,0:1SU f,2:1D x,8:1R x@2,11:1R x@2,13:1R x@2,15:1R x@2,20:1R f@0"); }

	@Test public void obj1() { test("{ string.cons(_, tail) = { length = 1 + tail.length }, empty = { }}", "2:6SU string,14:1U _,17:4D tail,40:4R tail@17"); }
	@Test public void obj2() { test("cons(_, tail) = { length = 1 + tail.length } ; empty = { }", "0:4SU cons,0:4U cons,5:1U _,8:4D tail,31:4R tail@8,47:5U empty"); }
	@Test public void obj3() { test("{self.a(b,c) = c.foo({ (d) = b + self }) }", "1:4D self,8:1D b,10:1D c,15:1R c@10,24:1U d,29:1R b@8,33:4R self@1"); }

	@Test public void obj4() { test("{(x + y) = ( x.a(y) => x ; x.b(x) => w )}",
			"2:1D x,6:1D y,13:1R x@2,17:1R y@6,23:1R x@2,27:1R x@2,31:1R x@2,37:1F w"); }

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
		SourceExpr parseResult;
		try {
			parseResult = new BanjoParser().parse(nonNull(src));
		} catch (final IOException e) {
			throw new UnexpectedIOExceptionError(e);
		}
		final DesugarResult<CoreExpr> dsResult = new BanjoDesugarer().desugar(parseResult);
		System.out.println("Desugar result:\n    "+dsResult.getValue());
		final DefRefAnalyser analyser = new DefRefAnalyser();
		final Analysis analysis = analyser.analyse(TEST_URI, dsResult.getValue(), parseResult.getSourceFileRange());
		TreeMap<FileRange,Set<String>> rangeCodes = TreeMap.empty(Ord.<FileRange>comparableOrd());
		for(final Key freeRef : analysis.getFree()) {
			final FileRange r = freeRef.getSourceFileRange().getFileRange();
			if(freeRef.getSourceFileRange() == SourceFileRange.SYNTHETIC) continue;
			final String id = freeRef.getKeyString();
			System.out.println("F "+id+" "+r);
			rangeCodes = addRangeCode(rangeCodes, r, "F "+id);
		}
		for(final P2<MethodParamDecl, Set<Key>> rp : analysis.getRefs()) {
			final FileRange r = rp._1().getSourceFileRange().getFileRange();
			if(rp._1().getSourceFileRange() == SourceFileRange.SYNTHETIC) continue;
			String id = rp._1().getName().getKeyString();
			for(final Key ref : rp._2()) {
				if(ref.getSourceFileRange() == SourceFileRange.SYNTHETIC) continue;
				final FileRange refRange = ref.getSourceFileRange().getFileRange();
				final String refText = src.substring(refRange.getStartOffset(), refRange.getEndOffset());
				if(!refText.equals(id)) id = id +" ("+refText+")";
				System.out.println("R "+id+" "+refRange+" -> "+r);
				rangeCodes = addRangeCode(rangeCodes, refRange, "R "+id+"@"+r.getStartOffset());
				rangeCodes = addRangeCode(rangeCodes, r, "D "+id);
			}
		}
		for(@NonNull @SuppressWarnings("null") final MethodParamDecl p : analysis.getUnusedDefs()) {
			final FileRange r = p.getSourceFileRange().getFileRange();
			if(p.getSourceFileRange() == SourceFileRange.SYNTHETIC) continue;
			final String id = p.getName().getKeyString();
			final boolean isSelfName = analysis.getSelfNames().member(p.getName());
			System.out.println("U "+id+" "+r);
			rangeCodes = addRangeCode(rangeCodes, r, (isSelfName?"SU ":"U ")+id);
		}
		for(@NonNull @SuppressWarnings("null") final MethodParamDecl p : analysis.getShadowingDefs()) {
			final FileRange r = p.getSourceFileRange().getFileRange();
			if(p.getSourceFileRange() == SourceFileRange.SYNTHETIC) continue;
			final String id = p.getName().getKeyString();
			final boolean isSelfName = analysis.getSelfNames().member(p.getName());
			System.out.println("S "+id+" "+r);
			rangeCodes = addRangeCode(rangeCodes, r, (isSelfName?"SS ":"S ")+id);
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
