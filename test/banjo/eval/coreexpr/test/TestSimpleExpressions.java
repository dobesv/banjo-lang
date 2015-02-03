package banjo.eval.coreexpr.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import banjo.dom.core.CoreExpr;
import banjo.dom.core.ObjectLiteral;
import banjo.eval.coreexpr.CoreExprEvaluator;

public class TestSimpleExpressions {
	public static boolean isTruthyExpr(String src) {
		System.out.println("Source: "+src);
		final ObjectLiteral value = CoreExprEvaluator.eval(src);
		if(CoreExprEvaluator.isFailure(value)) {
			System.out.println("FAILURE");
			return false;
		} else {
			System.out.println("Result: "+value.toSource());
			return CoreExprEvaluator.isTruthy(value);
		}
	}

	public static boolean isDefined(String src) {
		return !CoreExprEvaluator.isFailure(CoreExprEvaluator.eval(src));
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

	@Test public void testRange22() { assertTrue(isTruthyExpr("[1, 2, 3, 4, 5].range(2, 2) == [3, 4]")); }

}
