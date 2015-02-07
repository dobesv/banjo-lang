package banjo.eval.coreexpr.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import banjo.dom.core.CoreExpr;
import banjo.eval.coreexpr.CoreExprEvaluator;

public class TestSimpleExpressions {
	CoreExprEvaluator evaluator = CoreExprEvaluator.forSourceFile("(test)");
	public boolean isTruthyExpr(String src) {
		System.out.println("Source: "+src);
		final CoreExpr value = evaluator.evaluate(CoreExpr.fromString(src));
		if(evaluator.isFailure(value)) {
			System.out.println("FAILURE");
			return false;
		} else {
			CoreExpr simplified = evaluator.simplify(value);
			System.out.println("Result: "+simplified.toSource());
			return evaluator.isTruthy(value);
		}
	}

	public boolean isDefined(String src) {
		return !evaluator.isFailure(CoreExprEvaluator.eval(src));
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
