package banjo.idesupport.test;

import static banjo.dom.test.ParseTestUtils.test;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import banjo.desugar.IncrementalUpdater;
import banjo.dom.Expr;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.ExprList;
import banjo.dom.core.ObjectLiteral;
import banjo.parser.util.ParserReader;

public class TestIncrementalReparser {

	@Test public void test1() { testEdit("a = b; a", "foo = b; a", ExprList.class, 0, 1, "foo"); }
	@Test public void test2() { testEdit("a = b; a", "a = foo; a", ExprList.class, 4, 1, "foo"); }
	@Test public void test3() { testEdit("a = b; a", "a = b; foo", ExprList.class, 7, 1, "foo"); }
	@Test public void test4() { testEdit("a = b; a", "foo = bar; a", ExprList.class, 0, 5, "foo = bar"); }
	@Test public void test5() { testEdit("a = b; a", "a = foo; a", ExprList.class, 1, 4, " = foo"); }
	@Test public void test6() { testEdit("a = b; a", "{}", ExprList.class, 0, 8, "{}"); }
	@Test public void test7() { testEdit("a = b; a", "a = b.plus(a); a", ExprList.class, 5, 0, " + a"); }
	@Test public void test8() { testEdit("a = 1; a", "a = 12; a", ExprList.class, 5, 0, "2"); }
	@Test public void test9() { testEdit("a = 1; a", "a = 1.2; a", ExprList.class, 5, 0, ".2"); }
	@Test public void test10() { testEdit("{a: \"bla\"}", "{a: \"yay\"}", ObjectLiteral.class, 5, 3, "yay"); }
	@Test public void test11() { testEdit("{a: \"bla\"}", "{a: \"yay\"}", ObjectLiteral.class, 4, 4, "\"yay"); }
	@Test public void test12() { testEdit("{a: \"bla\"}", "{a: \"yay\"}", ObjectLiteral.class, 5, 4, "yay\""); }
	@Test public void test13() { testEdit("{a: \"bla\"}", "{a: \"yay\"}", ObjectLiteral.class, 4, 5, "\"yay\""); }
	@Test public void test14() { testEdit("{a: \"bla\"}", "{a: \"yay\"}", ObjectLiteral.class, 3, 6, " \"yay\""); }
	@Test public void test15() { testEdit("{a: \"bla\"}", "{a: \"yay\"}", ObjectLiteral.class, 3, 7, " \"yay\"}"); }

	public static <T extends CoreExpr> void testEdit(String origSource, String expectedNewSource,
			Class<T> origClass, int editOffset, int editLength,
			String replacement) {
		CoreExpr expr = test(origSource, origClass);
		if(expr == null) throw new NullPointerException();
		IncrementalUpdater updater = new IncrementalUpdater();
		ParserReader temp = ParserReader.fromSubstring("", origSource, editOffset, editOffset+editLength);
		int editStartLine = temp.getCurrentLineNumber();
		int editStartColumn = temp.getCurrentColumnNumber();
		String newSourceCode = new StringBuffer(origSource).replace(editOffset, editOffset + editLength, replacement).toString();
		System.out.println("Source code after replacing "+editLength+" chars at offset "+editOffset+" with string '"+replacement+"':");
		System.out.println("  "+newSourceCode);
		CoreExpr editedAst = updater.applyEdit(expr, editOffset, editLength, editStartLine, editStartColumn, replacement, newSourceCode);
		System.out.println("Normalized source code after replacing "+editLength+" chars at offset "+editOffset+" with string '"+replacement+"':");
		System.out.println("  "+editedAst.toSource());
		assertEquals(expectedNewSource, editedAst.toSource());
	}
}
