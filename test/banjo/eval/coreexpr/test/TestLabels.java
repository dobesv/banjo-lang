package banjo.eval.coreexpr.test;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import banjo.dom.core.CoreExpr;
import banjo.eval.coreexpr.CoreExprEvaluator;
import banjo.eval.util.JavaRuntimeSupport;

public class TestLabels {
	CoreExprEvaluator evaluator = CoreExprEvaluator.forSourceFile("(test)");
	public String stringRepr(String src) {
		System.out.println("Source: "+src);
		final CoreExpr ast = CoreExpr.fromString(src);
		final Object evalResult = evaluator.evaluate(ast);
		final Object value = JavaRuntimeSupport.force(evalResult);
		final String valueStr = value.toString();
		System.out.println("Result: "+valueStr);
		return valueStr;
	}

	@Test public void zero() { testRepr("0"); }
	@Test public void one() { testRepr("1"); }
	@Test public void two() { testRepr("2"); }
	@Test public void _true() { testRepr("true"); }
	@Test public void _false() { testRepr("false"); }
	@Test public void f1() { testRepr("x -> x", "<function from :line 1 col 1>"); }

	@Test public void obj1() { testRepr("{label = \"foo\"}", "foo"); }

	@Test public void ext1() { testRepr("{a=b} @ {label = \"bla\"}", "bla"); }
	@Test public void ext2() { testRepr("{label = \"bla\"} @ {a=b}", "bla"); }
	@Test public void ext3() { testRepr("{label = \"bla\"} @ (x -> x+1)", "bla"); }
	@Test public void ext4() { testRepr("(x -> x+1) @ {label = \"bla\"}", "bla"); }
	@Test public void ext5() { testRepr("(x -> x+1) @ {foo = \"bla\"}", "<object from :line 1 col 12>"); }

	@Test public void slot1() { testRepr("{ foo = { label = \"bar\" } }.foo", "bar"); }
	@Test public void slot2() { testRepr("{ foo = { }, label = \"bar\" }.foo", "{}"); }


	public void testRepr(String expr) {
		testRepr(expr, expr);
	}
	public void testRepr(String expr, String expectedRepr) {
		assertEquals(expectedRepr, stringRepr(expr));
	}
}
