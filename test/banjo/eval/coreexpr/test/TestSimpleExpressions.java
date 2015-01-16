package banjo.eval.coreexpr.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import banjo.eval.coreexpr.CoreExprEvaluator;
import banjo.eval.coreexpr.EvalResult;

public class TestSimpleExpressions {
	public static boolean isTruthyExpr(String src) {
		System.out.println("Source: "+src);
		final EvalResult value = CoreExprEvaluator.eval(src);
		System.out.println("Result: "+value.object.toSource());
		return value.isTruthy();
	}

	public static boolean isDefined(String src) {
		return CoreExprEvaluator.eval(src).isFailure();
	}

	@Test public void trueIsTruthy()     { assertTrue(isTruthyExpr("true")); }
	@Test public void trueEqTrue()       { assertTrue(isTruthyExpr("true == true")); }
	@Test public void trueNeqFalse()      { assertFalse(isTruthyExpr("true == false")); }
	@Test public void notTrueEqNotTrue()       { assertTrue(isTruthyExpr("! true == ! true")); }
	@Test public void notTrueEqFalse()       { assertTrue(isTruthyExpr("! true == false")); }
	@Test public void trueEqNotFalse()       { assertTrue(isTruthyExpr("true == ! false")); }

	@Test public void falseIsNotTruthy() { assertFalse(isTruthyExpr("false")); }
	@Test public void falseEqFalse()     { assertTrue(isTruthyExpr("false == false")); }
	@Test public void testFalseNeqTrue() { assertFalse(isTruthyExpr("false == true")); }
	@Test public void testNotFalseEqTrue() { assertTrue(isTruthyExpr("! false == true")); }
	@Test public void testFalseEqNotTrue() { assertTrue(isTruthyExpr("false == ! true")); }
	@Test public void testNotFalseEqNotFalse() { assertTrue(isTruthyExpr("! false == ! false")); }

}
