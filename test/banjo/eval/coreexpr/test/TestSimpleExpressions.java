package banjo.eval.coreexpr.test;

import static org.junit.Assert.*;

import org.junit.Test;

import banjo.eval.coreexpr.CoreExprEvaluator;

public class TestSimpleExpressions {
	public static boolean isTruthyExpr(String src) {
		return CoreExprEvaluator.eval(src).isTruthy();
	}

	public static boolean isDefined(String src) {
		return CoreExprEvaluator.eval(src).isFailure();
	}

	@Test public void trueIsTruthy()     { assertTrue(isTruthyExpr("true")); }
	@Test public void trueEqTrue()       { assertTrue(isTruthyExpr("true == true")); }
	@Test public void trueNeqFalse()      { assertFalse(isTruthyExpr("true == false")); }

	@Test public void falseIsNotTruthy() { assertFalse(isTruthyExpr("false")); }
	@Test public void falseEqFalse()     { assertTrue(isTruthyExpr("false == false")); }
	@Test public void testFalseNeqTrue() { assertFalse(isTruthyExpr("false == true")); }

}
