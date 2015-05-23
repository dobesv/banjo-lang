package banjo.eval.coreexpr.test;

import junit.framework.AssertionFailedError;
import junit.framework.TestResult;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.Extend;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.Let;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.core.Slot;
import banjo.dom.core.SlotReference;
import banjo.dom.source.Operator;
import banjo.dom.token.Identifier;
import banjo.eval.Fail;
import banjo.eval.ProjectLoader;
import banjo.eval.coreexpr.CoreExprEvaluator;
import banjo.eval.util.JavaRuntimeSupport;
import fj.P;
import fj.P2;
import fj.data.List;

@RunWith(Parameterized.class)
public class TestRunCoreLibraryExamples extends BaseExprTest {
	public TestRunCoreLibraryExamples(CoreExpr expr, CoreExpr withoutScope) {
	    super(expr);
    }

//	@Test
//	public void test() throws Throwable {
//		CoreExpr body = TestRunCoreLibraryExamples.stripScope(expr);
//		final CoreExprEvaluator evaluator = CoreExprEvaluator.forSourceFile(expr.getSourceFileRanges().head().getSourceFile());
//		Object evalResult = evaluator.evaluate(expr);
//		if(JavaRuntimeSupport.isDefined(evalResult)) {
//			if(JavaRuntimeSupport.isTruthy(evalResult)) {
//			} else {
//				final AssertionFailedError afe = new AssertionFailedError(body+" --> "+evalResult);
//				TestRunCoreLibraryExamples.setStackTrace(expr, afe);
//				throw afe;
//			}
//		} else if(evalResult instanceof Throwable) {
//			throw (Throwable) evalResult;
//		} else {
//			throw new Fail(body+" --> "+evalResult);
//		}
//	}
//
	public static final Identifier EXAMPLES_KEY = new Identifier("usage examples");

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

	static private List<CoreExpr> findExamples(CoreExpr base) {
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

			public List<CoreExpr> slot(Slot slot) {
				return slot.value.acceptVisitor(this);
			}
			@Override
			public List<CoreExpr> objectLiteral(ObjectLiteral n) {
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
			    return List.join(let.bindings.map(this::binding)).map(e -> new Let(let.getSourceFileRanges(), let.bindings, e));
			}
			@Override
			public List<CoreExpr> extend(Extend n) {
				return n.getBase().acceptVisitor(this).append(n.getExtension().acceptVisitor(this));
			}

			@Override
			public List<CoreExpr> functionLiteral(FunctionLiteral f) {
			    return f.body.acceptVisitor(this).map(e ->
			        f.sourceObjectBinding.map(recId ->
			            (CoreExpr)new Let(recId.getSourceFileRanges(), List.single(P.p(recId, f)), e))
		            .orSome(e));
			}

			@Override
			public List<CoreExpr> slotReference(SlotReference slotReference) {
			    return slotReference.object.acceptVisitor(this);
			}
		});
	}
	private static List<CoreExpr> allExamples;

	public static List<CoreExpr> getAllExamples() {
		if(allExamples == null) {
			allExamples = findAllExamples();
		}
		return allExamples;
	}

	public static List<CoreExpr> findAllExamples() {
		return List.join((new ProjectLoader()).loadBanjoPath()
				.map(P2.__2())
				.<List<CoreExpr>>map(TestRunCoreLibraryExamples::findExamples));
	}

	@Parameters(name="{1}")
	public static List<Object[]> parameters() {
		return findAllExamples().map(x -> new Object[]{x, stripScope(x)});
	}

	static CoreExpr stripScope(CoreExpr x) {
		while(x instanceof Let) {
			x = ((Let)x).body;
		}
		return x;
	}
	public static void setStackTrace(CoreExpr x, final AssertionFailedError afe) {
	    afe.setStackTrace(new StackTraceElement[] {
	    	new StackTraceElement("<examples>",
	    		x.toSource(),
	    		x.getSourceFileRanges().toOption().map(sfr -> sfr.getSourceFile()).toNull(),
	    		x.getSourceFileRanges().toOption().map(sfr -> sfr.getStartLine()).orSome(-1))
	    });
    }
}
