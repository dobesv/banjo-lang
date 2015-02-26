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

	public void assertTruthyExpr(String src) {
		assertTrue(src, isTruthyExpr(src));
	}

	public boolean isDefined(String src) {
		return !evaluator.isFailure(CoreExprEvaluator.eval(src));
	}

	@Test public void trueIsTruthy()     { assertTruthyExpr(("true")); }
	@Test public void trueEqTrue()       { assertTruthyExpr(("true == true")); }
	@Test public void trueNeqFalse()      { assertFalse(isTruthyExpr("true == false")); }
	@Test public void notTrueEqNotTrue()       { assertTruthyExpr(("! true == ! true")); }
	@Test public void notTrueEqFalse()       { assertTruthyExpr(("! true == false")); }
	@Test public void trueEqNotFalse()       { assertTruthyExpr(("true == ! false")); }

	@Test public void falseIsNotTruthy() { assertFalse(isTruthyExpr("false")); }
	@Test public void falseEqFalse()     { assertTruthyExpr(("false == false")); }
	@Test public void testFalseNeqTrue() { assertFalse(isTruthyExpr("false == true")); }
	@Test public void testNotFalseEqTrue() { assertTruthyExpr(("! false == true")); }
	@Test public void testFalseEqNotTrue() { assertTruthyExpr(("false == ! true")); }
	@Test public void testNotFalseEqNotFalse() { assertTruthyExpr(("! false == ! false")); }

	@Test public void smalIntegerComparisons() {
		for(int i = 1; i <= 10; i++) {
			assertTruthyExpr(String.valueOf(i)+"=="+String.valueOf(i));
			assertTruthyExpr(String.valueOf(-i)+"=="+String.valueOf(-i));
			assertTruthyExpr(String.valueOf(-i)+" < "+String.valueOf(i));
			assertTruthyExpr(String.valueOf(-i)+" <= "+String.valueOf(i));
			assertTruthyExpr(String.valueOf(-i)+" != "+String.valueOf(i));
			assertTruthyExpr(String.valueOf(i)+" >= "+String.valueOf(-i));
			assertTruthyExpr(String.valueOf(i)+" > "+String.valueOf(-i));
			assertTruthyExpr(String.valueOf(i)+" != "+String.valueOf(-i));
		}
	}

	@Test public void emptyListEqualsEmptyList() { assertTruthyExpr("[] == []"); }

	@Test public void zeroIsZero() { assertTruthyExpr(("0 . is zero")); }
	@Test public void eq0() { assertTruthyExpr(("0 == 0")); }
	@Test public void eq1() { assertTruthyExpr(("1 == 1")); }
	@Test public void eq2() { assertTruthyExpr(("2 == 2")); }
	@Test public void eq3() { assertTruthyExpr(("3 == 3")); }
	@Test public void eq4() { assertTruthyExpr(("3 == 3")); }
	@Test public void eq5() { assertTruthyExpr(("3 == 3")); }
	@Test public void minusOneEqualsMinusOne() { assertTruthyExpr(("-1 == -1")); }
	@Test public void minusTwoEqualsMinusTwo() { assertTruthyExpr(("-2 == -2")); }
	@Test public void minusThreeEqualsMinusThree() { assertTruthyExpr(("-3 == -3")); }


	@Test public void testRange22() { assertTruthyExpr(("[1, 2, 3, 4, 5].slice(2, 2) == [3, 4]")); }

}
