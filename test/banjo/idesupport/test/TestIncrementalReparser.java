package banjo.idesupport.test;

import static banjo.dom.test.ParseTestUtils.test;
import static banjo.parser.util.Check.nonNull;
import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.jdt.annotation.NonNull;
import org.junit.Test;

import banjo.desugar.BanjoDesugarer;
import banjo.desugar.BanjoDesugarer.DesugarResult;
import banjo.desugar.IncrementalUpdater;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.ObjectLiteral;
import banjo.parser.BanjoParser;
import banjo.parser.BanjoParser.ExtSourceExpr;
import banjo.parser.util.OffsetLength;
import banjo.parser.util.UnexpectedIOExceptionError;
import fj.P2;

public class TestIncrementalReparser {

	@Test public void test1() { testEdit("a = b; a", "{(foo) = a}(b)", Call.class, 0, 1, "foo", reg(0,10)); }
	@Test public void test2() { testEdit("a = b; a", "{(a) = a}(foo)", Call.class, 4, 1, "foo", reg(4,3)); }
	@Test public void test3() { testEdit("a = b; a", "{(a) = foo}(b)", Call.class, 7, 1, "foo", reg(7,3)); }
	@Test public void test4() { testEdit("a = b; a", "{(foo) = a}(bar)", Call.class, 0, 5, "foo = bar", reg(0,9)); }
	@Test public void test5() { testEdit("a = b; a", "{(a) = a}(foo)", Call.class, 1, 4, " = foo", reg(0,7)); }
	@Test public void test6() { testEdit("a = b; a", "{}", Call.class, 0, 8, "{}", reg(0,2)); }
	@Test public void test7() { testEdit("a = b; a", "{(a) = a}(b.\\+(a))", Call.class, 5, 0, " + a", reg(0,12)); }
	@Test public void test8() { testEdit("a = 1; a", "{(a) = a}(12)", Call.class, 5, 0, "2", reg(0,9)); }
	@Test public void test9() { testEdit("a = 1; a", "{(a) = a}(1.2)", Call.class, 5, 0, ".2", reg(0,10)); }
	@Test public void test10() { testEdit("{a = \"bla\"}", "{a = \"yay\"}", ObjectLiteral.class, 6, 3, "yay", reg(5,5)); }
	@Test public void test11() { testEdit("{a = \"bla\"}", "{a = \"yay\"}", ObjectLiteral.class, 5, 4, "\"yay", reg(1,9)); }
	@Test public void test12() { testEdit("{a = \"bla\"}", "{a = \"yay\"}", ObjectLiteral.class, 6, 4, "yay\"", reg(0,11)); }
	@Test public void test13() { testEdit("{a = \"bla\"}", "{a = \"yay\"}", ObjectLiteral.class, 5, 5, "\"yay\"", reg(1,9)); }
	@Test public void test14() { testEdit("{a = \"bla\"}", "{a = \"yay\"}", ObjectLiteral.class, 4, 6, " \"yay\"", reg(1,9)); }
	@Test public void test15() { testEdit("{a = \"bla\"}", "{a = \"yay\"}", ObjectLiteral.class, 4, 7, " \"yay\"}", reg(0,11)); }
	@Test public void test16() { testEdit("{abc = \"bla\"}", "{ac = \"bla\"}", ObjectLiteral.class, 2, 1, "", reg(1,2)); }
	@Test public void test17() { testEdit("{abc = \"bla\"}", "{f(a) = \"bla\"}", ObjectLiteral.class, 1, 3, "f(a)", reg(1,12)); }

	static OffsetLength reg(int offset, int length) { return new OffsetLength(offset, length); }

	public static <T extends CoreExpr> void testEdit(@NonNull String origSource, @NonNull String expectedNewSource,
			Class<T> origClass, int editOffset, int editLength,
			@NonNull String replacement, OffsetLength ... expectedDamage) {
		final CoreExpr expr = test(origSource, null, origClass);
		if(expr == null) throw new NullPointerException();
		final IncrementalUpdater updater = new IncrementalUpdater();
		ExtSourceExpr parseResult;
		try {
			parseResult = new BanjoParser().parse(origSource);
		} catch (final IOException e) {
			throw new UnexpectedIOExceptionError(e);
		}
		final DesugarResult<CoreExpr> dsResult = new BanjoDesugarer(parseResult.getSourceMap()).desugar(parseResult.getExpr());
		final String newSourceCode = nonNull(new StringBuffer(origSource).replace(editOffset, editOffset + editLength, replacement).toString());
		System.out.println("Source code after replacing "+editLength+" chars at offset "+editOffset+" with string '"+replacement+"':");
		System.out.println("  "+newSourceCode);
		final List<OffsetLength> damageRegions = new ArrayList<>();
		final P2<ExtSourceExpr, DesugarResult<CoreExpr>> edited = updater.applyEdit(dsResult, parseResult, editOffset, editLength, replacement, newSourceCode, damageRegions);
		final DesugarResult<CoreExpr> editedAst = edited._2();
		System.out.println("Normalized source code after replacing "+editLength+" chars at offset "+editOffset+" with string '"+replacement+"':");
		System.out.println("  "+editedAst.getValue().toSource());
		System.out.println("Damage regions:");
		for(final OffsetLength region : damageRegions) {
			System.out.println("  "+region+(region.getOffset()==0&&region.getLength()==newSourceCode.length()?"(entire source)":"(\""+newSourceCode.substring(region.getOffset(), region.getEndOffset())+"\")"));
		}
		assertEquals(Arrays.asList(expectedDamage).toString(), damageRegions.toString());
		assertEquals(expectedNewSource, editedAst.getValue().toSource());
	}
}
