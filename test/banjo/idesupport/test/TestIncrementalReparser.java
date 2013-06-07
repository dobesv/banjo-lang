package banjo.idesupport.test;

import static banjo.dom.test.ParseTestUtils.test;
import static banjo.parser.util.Check.nonNull;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.jdt.annotation.NonNull;
import org.junit.Test;

import banjo.desugar.IncrementalUpdater;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.ExprList;
import banjo.dom.core.ObjectLiteral;
import banjo.parser.errors.Problem;
import banjo.parser.util.OffsetLength;

public class TestIncrementalReparser {

	@Test public void test1() { testEdit("a = b; a", "foo = b; a", ExprList.class, 0, 1, "foo", reg(0,10)); }
	@Test public void test2() { testEdit("a = b; a", "a = foo; a", ExprList.class, 4, 1, "foo", reg(0,10)); }
	@Test public void test3() { testEdit("a = b; a", "a = b; foo", ExprList.class, 7, 1, "foo"); }
	@Test public void test4() { testEdit("a = b; a", "foo = bar; a", ExprList.class, 0, 5, "foo = bar"); }
	@Test public void test5() { testEdit("a = b; a", "a = foo; a", ExprList.class, 1, 4, " = foo"); }
	@Test public void test6() { testEdit("a = b; a", "{}", ExprList.class, 0, 8, "{}"); }
	@Test public void test7() { testEdit("a = b; a", "a = b.\\+(a); a", ExprList.class, 5, 0, " + a"); }
	@Test public void test8() { testEdit("a = 1; a", "a = 12; a", ExprList.class, 5, 0, "2"); }
	@Test public void test9() { testEdit("a = 1; a", "a = 1.2; a", ExprList.class, 5, 0, ".2"); }
	@Test public void test10() { testEdit("{a: \"bla\"}", "{a: \"yay\"}", ObjectLiteral.class, 5, 3, "yay", reg(4,5)); }
	@Test public void test11() { testEdit("{a: \"bla\"}", "{a: \"yay\"}", ObjectLiteral.class, 4, 4, "\"yay"); }
	@Test public void test12() { testEdit("{a: \"bla\"}", "{a: \"yay\"}", ObjectLiteral.class, 5, 4, "yay\""); }
	@Test public void test13() { testEdit("{a: \"bla\"}", "{a: \"yay\"}", ObjectLiteral.class, 4, 5, "\"yay\""); }
	@Test public void test14() { testEdit("{a: \"bla\"}", "{a: \"yay\"}", ObjectLiteral.class, 3, 6, " \"yay\""); }
	@Test public void test15() { testEdit("{a: \"bla\"}", "{a: \"yay\"}", ObjectLiteral.class, 3, 7, " \"yay\"}"); }

	static OffsetLength reg(int offset, int length) { return new OffsetLength(offset, length); }

	public static <T extends CoreExpr> void testEdit(@NonNull String origSource, @NonNull String expectedNewSource,
			Class<T> origClass, int editOffset, int editLength,
			@NonNull String replacement, OffsetLength ... expectedDamage) {
		final CoreExpr expr = test(origSource, origClass);
		if(expr == null) throw new NullPointerException();
		final IncrementalUpdater updater = new IncrementalUpdater();
		final String newSourceCode = nonNull(new StringBuffer(origSource).replace(editOffset, editOffset + editLength, replacement).toString());
		System.out.println("Source code after replacing "+editLength+" chars at offset "+editOffset+" with string '"+replacement+"':");
		System.out.println("  "+newSourceCode);
		final List<Problem> problems = new ArrayList<>();
		final List<OffsetLength> damageRegions = new ArrayList<>();
		final CoreExpr editedAst = updater.applyEdit(expr, editOffset, editLength, replacement, newSourceCode, problems, damageRegions);
		System.out.println("Normalized source code after replacing "+editLength+" chars at offset "+editOffset+" with string '"+replacement+"':");
		System.out.println("  "+editedAst.toSource());
		System.out.println("Damage regions:");
		for(final OffsetLength region : damageRegions) {
			System.out.println("  "+region+(region.getOffset()==0&&region.getLength()==newSourceCode.length()?"(entire source)":"(\""+newSourceCode.substring(region.getOffset(), region.getEnd())+"\")"));
		}
		if(expectedDamage.length > 0)
			assertEquals(Arrays.asList(expectedDamage).toString(), damageRegions.toString());
		assertEquals(expectedNewSource, editedAst.toSource());
	}
}
