package banjo.eval.coreexpr.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.Extend;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.Let;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.core.SlotReference;
import banjo.dom.source.Operator;
import banjo.dom.token.Identifier;
import banjo.eval.ProjectLoader;
import banjo.eval.coreexpr.CoreExprEvaluator;
import fj.P2;
import fj.data.List;

public class TestRunCoreLibraryExamples {
	Identifier EXAMPLES_KEY = new Identifier("usage examples");

	static Boolean looksLikeExample(CoreExpr arg) {
		return arg.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
			@Override
			public Boolean fallback() {
				return false;
			}

			@Override
			public Boolean call(Call n) {
				final Operator op = n.getBinaryOperator().toNull();
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

		final CoreExprEvaluator evaluator = CoreExprEvaluator.forSourceFile(x.getSourceFileRanges().head().getSourceFile());
		CoreExpr result = evaluator.evaluate(x);
		final boolean working = !evaluator.isFailure(result);
		//if(!working) System.out.println("Error: "+result.object);
		final boolean success = working && evaluator.isTruthy(result);
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
				return call.target.acceptVisitor(this).append(List.join(call.args.map(arg -> arg.acceptVisitor(this))));
			}

			@Override
			public List<CoreExpr> listLiteral(
					ListLiteral n) {
				return List.join(n.getElements().<List<CoreExpr>>map(elt -> elt.acceptVisitor(this)));

			}

			public List<CoreExpr> slot(P2<Identifier,CoreExpr> slot) {
				return slot._1().acceptVisitor(this);
			}
			@Override
			public List<CoreExpr> objectLiteral(
					ObjectLiteral n) {
				final List<List<CoreExpr>> methodExamples = n.getSlots().<List<CoreExpr>>map(this::slot);
				return List.<CoreExpr>join(methodExamples);
			}

			public List<CoreExpr> binding(P2<Identifier,CoreExpr> binding) {
				if(binding._1().eql(EXAMPLES_KEY) && binding._2() instanceof ListLiteral)
					return ((ListLiteral)binding._2()).getElements();
				return binding._2().acceptVisitor(this);
			}
			@Override
			public List<CoreExpr> let(Let let) {
			    return List.join(let.bindings.map(this::binding));
			}
			@Override
			public List<CoreExpr> extend(Extend n) {
				return n.getBase().acceptVisitor(this).append(n.getExtension().acceptVisitor(this));
			}

			@Override
			public List<CoreExpr> functionLiteral(FunctionLiteral f) {
			    return f.body.acceptVisitor(this);
			}

			@Override
			public List<CoreExpr> slotReference(SlotReference slotReference) {
			    return slotReference.object.acceptVisitor(this);
			}
		});
	}

	@Test
	public void testCoreLibraryExamplesPass() {

		List<CoreExpr> allExamples = List.join((new ProjectLoader()).loadBanjoPath()
			.values()
			.<List<CoreExpr>>map(this::findExamples));
		System.out.println("Found "+allExamples.length()+" examples");

		assertFalse("Failed to find any examples in the core library.", allExamples.isEmpty());
		final List<CoreExpr> failures = allExamples.filter(this::testFails);
		System.out.println("Found "+allExamples.length()+" examples, "+failures.length()+" failed");
		assertTrue(failures.isEmpty());
	}
}
