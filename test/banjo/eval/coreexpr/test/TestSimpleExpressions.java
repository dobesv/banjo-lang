package banjo.eval.coreexpr.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import fj.P;
import fj.P1;
import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.SlotReference;
import banjo.eval.coreexpr.CoreExprEvaluator;
import banjo.eval.util.JavaRuntimeSupport;

public class TestSimpleExpressions {
	CoreExprEvaluator evaluator = CoreExprEvaluator.forSourceFile("(test)");
	public boolean isTruthyExpr(String src) {
		System.out.println("Source: "+src);
		final CoreExpr ast = CoreExpr.fromString(src);
		final Object evalResult = evaluator.evaluate(ast);
		final Object value = JavaRuntimeSupport.force(evalResult);
		final String valueStr = value.toString();
		System.out.println("Result: "+valueStr);
		ast.acceptVisitor(new BaseCoreExprVisitor<Void>() {
			@Override
			public Void fallback() {
			    return null;
			}

			@Override
			public Void call(Call n) {
				if(n.target instanceof SlotReference) {
					final SlotReference methodReceiver = (SlotReference)n.target;
					final Object lhs = evaluator.evaluate(methodReceiver.object);
					n.getBinaryOperator().map(x -> {
						final Object rhs = evaluator.evaluate(n.args.head());
						System.out.println("Calling: "+lhs+" "+x.getOp()+" "+rhs);
						return null;
					}).orSome(P.lazy(() -> {
						System.out.println("Calling: "+lhs+"."+methodReceiver.slotName+"("+n.args.map(evaluator::evaluate)+")");
						return null;
					}));
				}
			    return null;
			}
		});
		if(!JavaRuntimeSupport.isDefined(value)) {
			final Throwable valueThrowable = (value instanceof Throwable) ? (Throwable) value : null;
			throw new AssertionError(src + " not truthy, got: " + valueStr, valueThrowable);
		} else {
			return JavaRuntimeSupport.isTruthy(value);
		}
	}

	public void assertTruthyExpr(String src) {
		assertTrue(src, isTruthyExpr(src));
	}

	public boolean isDefined(String src) {
		return JavaRuntimeSupport.isDefined(CoreExprEvaluator.eval(src));
	}

	@Test public void trueIsTruthy()     { assertTruthyExpr(("true")); }
	@Test public void trueEqTrue()       { assertTruthyExpr(("true == true")); }
	@Test public void trueNeqFalse()      { assertFalse(isTruthyExpr("true == false")); }
	@Test public void notTrueEqNotTrue()       { assertTruthyExpr(("! true == ! true")); }
	@Test public void notTrueEqFalse()       { assertTruthyExpr(("! true == false")); }
	@Test public void trueEqNotFalse()       { assertTruthyExpr(("true == ! false")); }
	@Test public void trueAndTrue()       { assertTruthyExpr(("true && true")); }
	@Test public void trueAndNotFalse()       { assertTruthyExpr(("true && !false")); }
	@Test public void falseOrTrue()       { assertTruthyExpr(("false || true")); }

	@Test public void falseIsNotTruthy() { assertFalse(isTruthyExpr("false")); }
	@Test public void falseEqFalse()     { assertTruthyExpr(("false == false")); }
	@Test public void testFalseNeqTrue() { assertFalse(isTruthyExpr("false == true")); }
	@Test public void testNotFalseEqTrue() { assertTruthyExpr(("! false == true")); }
	@Test public void testFalseEqNotTrue() { assertTruthyExpr(("false == ! true")); }
	@Test public void testNotFalseEqNotFalse() { assertTruthyExpr(("! false == ! false")); }

	@Test public void smallIntegerComparisons() {
		for(int i = 1; i <= 10; i++) {
			assertTruthyExpr(String.valueOf(i)+" == "+String.valueOf(i));
			assertTruthyExpr(String.valueOf(-i)+" == "+String.valueOf(-i));
			assertTruthyExpr(String.valueOf(-i)+" < "+String.valueOf(i));
			assertTruthyExpr(String.valueOf(-i)+" <= "+String.valueOf(i));
			assertTruthyExpr(String.valueOf(-i)+" != "+String.valueOf(i));
			assertTruthyExpr(String.valueOf(i)+" >= "+String.valueOf(-i));
			assertTruthyExpr(String.valueOf(i)+" > "+String.valueOf(-i));
			assertTruthyExpr(String.valueOf(i)+" != "+String.valueOf(-i));
		}
	}

	@Test public void emptyListEqualsEmptyList() { assertTruthyExpr("[] == []"); }
	@Test public void singleListEqualsItself1() { assertTruthyExpr("[true] == [true]"); }
	@Test public void singleListEqualsItself2() { assertTruthyExpr("[false] == [false]"); }
	@Test public void singleListNotEqualsEmptyList1() { assertTruthyExpr("[false] != []"); }
	@Test public void singleListNotEqualsEmptyList2() { assertTruthyExpr("[] != [false]"); }
	@Test public void emptyListIsEmpty() { assertTruthyExpr("[].is empty"); }

	@Test public void zeroIsZero() { assertTruthyExpr(("0 . is zero")); }
	@Test public void eq0() { assertTruthyExpr(("0 == 0")); }
	@Test public void eq1() { assertTruthyExpr(("1 == 1")); }
	@Test public void eq2() { assertTruthyExpr(("2 == 2")); }
	@Test public void eq3() { assertTruthyExpr(("3 == 3")); }
	@Test public void eq4() { assertTruthyExpr(("4 == 4")); }
	@Test public void eq5() { assertTruthyExpr(("5 == 5")); }
	@Test public void minusOneEqualsMinusOne() { assertTruthyExpr(("-1 == -1")); }
	@Test public void minusTwoEqualsMinusTwo() { assertTruthyExpr(("-2 == -2")); }
	@Test public void minusThreeEqualsMinusThree() { assertTruthyExpr(("-3 == -3")); }

	@Test public void testSlice24() { assertTruthyExpr(("[1, 2, 3, 4, 5].slice(2, 4) == [3, 4]")); }
	@Test public void testSlice22() { assertTruthyExpr(("[1, 2, 3, 4, 5].slice(2, 2) == []")); }
	@Test public void testSlice2Neg1() { assertTruthyExpr(("[1, 2, 3, 4, 5, 6].slice(2, -1) == [3, 4, 5]")); }

	@Test public void compose1() { assertTruthyExpr("(not = (x -> !x), not not = not ; not) ⇒ not not(true)"); }
	@Test public void compose2() { assertTruthyExpr("(not = (x -> !x), not not = not ; not, not not not = not not ; not) ⇒ not not not(true) == false"); }


}
