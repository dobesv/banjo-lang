package banjo.eval.coreexpr.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Ignore;
import org.junit.Test;

import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.Extend;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.Method;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.source.Operator;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.eval.ProjectLoader;
import banjo.eval.coreexpr.CoreExprEvaluator;
import banjo.eval.coreexpr.EvalResult;
import fj.data.List;

public class TestRunCoreLibraryExamples {
	Key EXAMPLES_KEY = new Identifier("examples");

	static Boolean looksLikeExample(CoreExpr arg) {
		return arg.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
			@Override
			public Boolean fallback() {
				return false;
			}

			@Override
			public Boolean call(Call n) {
				final Operator op = Operator.fromMethodName(n.getName(), true);
				if(op == null) {
					return false;
				}
				switch(op) {
				case EQ:
				case NEQ:
				case GT:
				case GE:
				case LT:
				case LE:
					return true;
				default:
					return false;
				}
			}
		});
	}
	static Boolean looksLikeExamplesList(CoreExpr arg) {
		return arg.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
			@Override
			public Boolean fallback() {
				return false;
			}

			@Override
			public Boolean listLiteral(ListLiteral n) {
				return n.getElements().isNotEmpty() && n.getElements().find(x -> !looksLikeExample(x)).isNone();
			}
		});
	}

	Boolean testFails(CoreExpr x) {
		System.out.println("Testing: "+x);

		EvalResult result = CoreExprEvaluator.forSourceFile(x.getSourceFileRanges().head().getSourceFile()).evaluate(x);
		final boolean working = !result.isFailure();
		if(!working) System.out.println("Error: "+result.object);
		final boolean success = working && result.isTruthy();
		System.out.println((working?success?"PASS":"FAIL":"ERROR")+": "+x);
		return ! success;
	}
	private List<CoreExpr> findExamples(CoreExpr base) {
		return base.acceptVisitor(new BaseCoreExprVisitor<List<CoreExpr>>() {
			@Override
			public List<CoreExpr> fallback() {
				return List.nil();
			}

			@Override
			public List<CoreExpr> call(Call call) {
				List<CoreExpr> examples;
				if(call.getArgumentLists().isSingle()
						&& call.getArgumentLists().head().isSingle()
						&& looksLikeExamplesList(call.getArgumentLists().head().head())) {
					examples = call.getObject().acceptVisitor(new BaseCoreExprVisitor<List<CoreExpr>>() {
						@Override
						public List<CoreExpr> objectLiteral(ObjectLiteral objectLiteral) {
							if(objectLiteral.isLambda()) {
								Method m = objectLiteral.findMethod(Key.ANONYMOUS);
								if(m != null && m.getArgumentLists().isSingle() && m.getArgumentLists().head().isSingle() && m.getArgumentLists().head().head().compareTo(EXAMPLES_KEY) == 0) {
									return ((ListLiteral)call.getArgumentLists().head().head()).getElements();
								}
							}
							return fallback();
						}

						@Override
						public List<CoreExpr> fallback() {
							return List.nil();
						}
					});
				} else {
					examples = List.nil();
				}
				final List<CoreExpr> argExamples = List.<CoreExpr>join(
							call.getAllArguments()
							.<List<CoreExpr>>map(a -> a.acceptVisitor(this))
				);
				return examples
						.append(call.getObject().acceptVisitor(this))
						.append(argExamples);
			}

			@Override
			public List<CoreExpr> listLiteral(
					ListLiteral n) {
				return List.join(n.getElements().<List<CoreExpr>>map(elt -> elt.acceptVisitor(this)));
			}

			@Override
			public List<CoreExpr> objectLiteral(
					ObjectLiteral n) {
				final List<List<CoreExpr>> methodExamples = n.getMethods().<List<CoreExpr>>map(method ->
					method.getBody().acceptVisitor(this)
				);
				return List.<CoreExpr>join(methodExamples);
			}

			@Override
			public List<CoreExpr> extend(Extend n) {
				return n.getBase().acceptVisitor(this).append(n.getExtension().acceptVisitor(this));
			}
		});
	}
	@Ignore public void testCoreLibraryExamplesPass() {

		List<CoreExpr> allExamples = List.join(ProjectLoader.loadBanjoPath()
			.values()
			.<List<CoreExpr>>map(this::findExamples));
		System.out.println("Found "+allExamples.length()+" examples");

		assertFalse("Failed to find any examples in the core library.", allExamples.isEmpty());
		final List<CoreExpr> failures = allExamples.filter(this::testFails);
		System.out.println("Get "+failures.length()+" failures");
		assertTrue(failures.isEmpty());
	}
}
