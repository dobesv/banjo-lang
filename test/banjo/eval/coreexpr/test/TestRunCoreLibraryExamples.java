package banjo.eval.coreexpr.test;

import static org.junit.Assert.*;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Test;

import fj.data.List;
import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprAlgebra;
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
import banjo.parser.util.SourceFileRange;

public class TestRunCoreLibraryExamples {
	Key EXAMPLES_KEY = new Identifier("examples");

	static Boolean looksLikeExample(CoreExpr arg) {
		return arg.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
			@Override
			public @NonNull Boolean fallback() {
				return false;
			}

			@Override
			public @NonNull Boolean call(@NonNull Call n) {
				final @Nullable Operator op = Operator.fromMethodName(n.getName(), true);
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
			public @NonNull Boolean fallback() {
				return false;
			}

			@Override
			public @NonNull Boolean listLiteral(@NonNull ListLiteral n) {
				return n.getElements().isNotEmpty() && n.getElements().find(x -> !looksLikeExample(x)).isNone();
			}
		});
	}

	Boolean testFails(CoreExpr x) {
		System.out.println("Testing: "+x);
		@NonNull
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
			public @NonNull List<@NonNull CoreExpr> fallback() {
				return List.nil();
			}

			@Override
			public @NonNull List<@NonNull CoreExpr> call(@NonNull Call call) {
				List<CoreExpr> examples;
				if(call.getArgumentLists().isSingle()
						&& call.getArgumentLists().head().isSingle()
						&& looksLikeExamplesList(call.getArgumentLists().head().head())) {
					examples = call.getObject().acceptVisitor(new BaseCoreExprVisitor<List<CoreExpr>>() {
						@Override
						public @NonNull List<@NonNull CoreExpr> objectLiteral(@NonNull ObjectLiteral objectLiteral) {
							if(objectLiteral.isLambda()) {
								Method m = objectLiteral.findMethod(Key.ANONYMOUS);
								if(m != null && m.getArgumentLists().isSingle() && m.getArgumentLists().head().isSingle() && m.getArgumentLists().head().head().compareTo(EXAMPLES_KEY) == 0) {
									return ((ListLiteral)call.getArgumentLists().head().head()).getElements();
								}
							}
							return fallback();
						}

						@Override
						public @NonNull List<@NonNull CoreExpr> fallback() {
							return List.nil();
						}
					});
				} else {
					examples = List.nil();
				}
				final @NonNull List<@NonNull CoreExpr> argExamples = call.getArgumentLists().foldLeft((sum, args) -> sum.append(List.join(args.map(a -> a.acceptVisitor(this)))), List.nil());
				return examples
						.append(call.getObject().acceptVisitor(this))
						.append(argExamples);
			}

			@Override
			public @NonNull List<@NonNull CoreExpr> listLiteral(
					@NonNull ListLiteral n) {
				return List.join(n.getElements().map(elt -> elt.acceptVisitor(this)));
			}

			@Override
			public @NonNull List<@NonNull CoreExpr> objectLiteral(
					@NonNull ObjectLiteral n) {
				return List.join(n.getMethods().map(method ->
					method.getBody().acceptVisitor(this)
				));
			}

			@Override
			public @NonNull List<@NonNull CoreExpr> extend(@NonNull Extend n) {
				return n.getBase().acceptVisitor(this).append(n.getExtension().acceptVisitor(this));
			}
		});
	}
	@Test public void testCoreLibraryExamplesPass() {
		@NonNull
		List<CoreExpr> allExamples = List.join(ProjectLoader.loadBanjoPath()
			.values()
			.map(this::findExamples));
		System.out.println("Found "+allExamples.length()+" examples");

		assertFalse("Failed to find any examples in the core library.", allExamples.isEmpty());
		final @NonNull List<@NonNull CoreExpr> failures = allExamples.filter(this::testFails);
		System.out.println("Get "+failures.length()+" failures");
		assertTrue(failures.isEmpty());
	}
}
