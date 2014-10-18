package banjo.eval.rhino.test;

import static banjo.parser.util.Check.nonNull;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static banjo.eval.rhino.RhinoTranslator.jsIdentifier;

import org.junit.Test;
import org.mozilla.javascript.NativeObject;

import banjo.dom.core.CoreExpr;
import banjo.eval.rhino.RhinoEvaluator;
import banjo.eval.rhino.RhinoTranslator;
import banjo.parser.test.ParseTestUtils;

public class TestSimpleExpressions {
	final static RhinoEvaluator evaluator = new RhinoEvaluator();
	public static Object evalExpr(String source) {
		CoreExpr expr = nonNull(ParseTestUtils.toCoreExpr(source));
		System.out.println("JS version:\n"+new RhinoTranslator().translate(expr).toSource(2));
		Object res = evaluator.evaluate(expr);
		System.out.println("Returned object:\n  "+res);
		return res;
	}

	@Test
	public void testEmptyObject() {
		Object r = evalExpr("{}");
		assertTrue(r instanceof NativeObject);
	}

	@Test
	public void testObjectWithOneMethod() {
		NativeObject r = (NativeObject)evalExpr("{one = {}}");
		Object one = r.get(jsIdentifier("one"), r);
		assertNotNull(one);
	}

	@Test
	public void testEmptyList() {
		Object r = evalExpr("[]");
	}

	@Test
	public void testStringList() {
		Object r = evalExpr("[\"a\", \"b\", \"c\"]");
	}

	@Test
	public void testEmptyString() {
		Object r = evalExpr("\"\"");
	}

	@Test
	public void testHelloString() {
		Object r = evalExpr("\"hello\"");
	}

	@Test
	public void testLet() {
		Object r = evalExpr("(s = \"hello\") => s");
	}
}
