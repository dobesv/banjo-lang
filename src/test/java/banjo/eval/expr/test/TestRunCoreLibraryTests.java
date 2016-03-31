package banjo.eval.expr.test;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import banjo.expr.core.BadCoreExpr;
import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.Call;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprFactory;
import banjo.expr.core.Extend;
import banjo.expr.core.FunctionLiteral;
import banjo.expr.core.Let;
import banjo.expr.core.ListLiteral;
import banjo.expr.core.ObjectLiteral;
import banjo.expr.core.Projection;
import banjo.expr.core.Slot;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Set;
import junit.framework.AssertionFailedError;

@RunWith(Parameterized.class)
public class TestRunCoreLibraryTests extends BaseExprTest {
	private CoreExpr withoutScope;

	public TestRunCoreLibraryTests(CoreExpr expr, CoreExpr withoutScope) {
	    this.expr = expr;
	    this.withoutScope = withoutScope;
    }

	@Override
	public CoreExpr getAst() {
		return expr;
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
    public static final Identifier TESTS_KEY = new Identifier("unit tests");

    static private List<CoreExpr> findTests(CoreExpr base) {
		return base.acceptVisitor(new BaseCoreExprVisitor<List<CoreExpr>>() {
			@Override
			public List<CoreExpr> fallback() {
				return List.nil();
			}

			@Override
			public List<CoreExpr> call(Call call) {
                if(call.target.eql(TESTS_KEY)) {
                    return List.join(call.args.map(arg -> arg.acceptVisitor(new BaseCoreExprVisitor<List<CoreExpr>>() {
                        @Override
                        public List<CoreExpr> fallback() {
                            return List.single(arg);
                        }

                        @Override
                        public List<CoreExpr> listLiteral(ListLiteral n) {
                            return n.elements;
                        }

                    })));
                }
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
                if(binding._1().eql(TESTS_KEY) && binding._2() instanceof ListLiteral)
					return ((ListLiteral)binding._2()).getElements();
				return binding._2().acceptVisitor(this);
			}
			@Override
			public List<CoreExpr> let(Let let) {
			    List<List<CoreExpr>> examplesInBindings = let.bindings.map(this::binding);
				List<CoreExpr> examplesInBody = let.body.acceptVisitor(this);
				return List.join(examplesInBindings).append(examplesInBody).map(e -> new Let(let.getSourceFileRanges(), let.bindings, e));
			}
			@Override
			public List<CoreExpr> extend(Extend n) {
				return n.getBase().acceptVisitor(this).append(n.getExtension().acceptVisitor(this));
			}

			@Override
			public List<CoreExpr> functionLiteral(FunctionLiteral f) {
			    List<CoreExpr> result = f.body.acceptVisitor(this).map(e ->
			        f.sourceObjectBinding.map(recId ->
			            (CoreExpr)new Let(recId.getSourceFileRanges(), List.single(P.p(recId, f)), e))
		            .orSome(e));
				return result;
			}

			
			@Override
			public List<CoreExpr> projection(Projection projection) {
			    return projection.object.acceptVisitor(this);
			}
			
            @Override
            public List<CoreExpr> badExpr(BadCoreExpr badExpr) {
                return List.single(badExpr);
            }
		});
	}
	private static List<CoreExpr> allTests;

	public static List<CoreExpr> getAllTests() {
		if(allTests == null) {
			allTests = findAllTests();
		}
		return allTests;
	}

	public static List<CoreExpr> findAllTests() {
        List<Path> paths = CoreExprFactory.projectSourcePathsForFile(Paths.get("src/test/banjo/").toAbsolutePath());
        CoreExpr ast = CoreExprFactory.INSTANCE.loadFromDirectories(paths);
        return findTests(ast);
	}

	@Parameters(name="{1}")
	public static List<Object[]> parameters() {
		return findAllTests().map(x -> new Object[]{x, stripScope(x)});
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
	    		x.getSourceFileRanges().toStream().toOption().map(sfr -> sfr.getSourceFile().toString()).toNull(),
	    		x.getSourceFileRanges().toStream().toOption().map(sfr -> sfr.getStartLine()).orSome(-1))
	    });
    }
	
	@Override
	public String exprSource() {
		return withoutScope.toSource();
	}
	
	@Override
	public Set<SourceFileRange> exprRanges() {
		return withoutScope.getSourceFileRanges();
	}
}
