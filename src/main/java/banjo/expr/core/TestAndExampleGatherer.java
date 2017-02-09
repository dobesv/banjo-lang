package banjo.expr.core;

import banjo.expr.token.Identifier;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Set;

public class TestAndExampleGatherer {

    private static class TestFindingCoreExprVisitor extends BaseCoreExprVisitor<Set<CoreExpr>> {
        @Override
        public Set<CoreExpr> fallback() {
            return CoreExpr.EMPTY_SET;
        }

        @Override
        public Set<CoreExpr> call(Call call) {
            Set<CoreExpr> argTests = union(call.args.map(this::visit));
            return call.target.acceptVisitor(this).union(argTests);
        }

        public Set<CoreExpr> tests(List<CoreExpr> tests) {
            return union(tests.map(arg -> arg.acceptVisitor(new BaseCoreExprVisitor<Set<CoreExpr>>() {
                @Override
                public Set<CoreExpr> fallback() {
                    return Set.single(CoreExprOrd.ORD, arg);
                }

                @Override
                public Set<CoreExpr> listLiteral(ListLiteral n) {
                    return Set.iterableSet(CoreExprOrd.ORD, n.elements);
                }
            })));
        }

        @Override
        public Set<CoreExpr> listLiteral(
            ListLiteral n) {
            List<Set<CoreExpr>> eltTests = n.elements.map(this::visit);
            return union(eltTests);

        }

        @Override
        public Set<CoreExpr> objectLiteral(ObjectLiteral n) {
            final List<Set<CoreExpr>> methodExamples = n.getSlots().map(Slot::getValue).map(this::visit);
            return union(methodExamples);
        }

        public Set<CoreExpr> binding(P2<Identifier, CoreExpr> binding) {
            return binding._2().acceptVisitor(this);
        }

        @Override
        public Set<CoreExpr> let(Let let) {
            Set<CoreExpr> examplesInBindings = union(let.bindings.map(this::binding));
            Set<CoreExpr> examplesInBody = let.body.acceptVisitor(this);
            Set<CoreExpr> allExamples = examplesInBindings.union(examplesInBody);
            // Need to rewrap the example in the let so it has whatever
            // variables from scope that it needs
            return allExamples.map(CoreExprOrd.ORD, e -> new Let(let.getRanges(), let.bindings, e));
        }

        @Override
        public Set<CoreExpr> extend(Extend n) {
            return n.getBase().acceptVisitor(this).union(n.getExtension().acceptVisitor(this));
        }

        @Override
        public Set<CoreExpr> functionLiteral(FunctionLiteral f) {
            Set<CoreExpr> result =
                f.body.acceptVisitor(this).map(
                    CoreExprOrd.ORD,
                    e -> f.calleeBinding.map(recId -> (CoreExpr) new Let(recId.getRanges(), List.single(P.p(recId, f)), e)).orSome(e));
            return result;
        }

        @Override
        public Set<CoreExpr> projection(Projection projection) {
            return projection.object.acceptVisitor(this);
        }
    }

    public static final Identifier TESTS_KEY = new Identifier("unit tests");
    public static final Identifier EXAMPLES_KEY = new Identifier("usage examples");

    public static Set<CoreExpr> union(List<Set<CoreExpr>> listOfTestSets) {
        return listOfTestSets.foldLeft((result, tests) -> result.union(tests), CoreExpr.EMPTY_SET);
    }

    static public Set<CoreExpr> findTests(CoreExpr base) {
        return base.acceptVisitor(new TestFindingCoreExprVisitor() {
            @Override
            public Set<CoreExpr> binding(P2<Identifier, CoreExpr> binding) {
                if(binding._1().id.equals(TESTS_KEY.id))
                    return tests(List.single(binding._2()));
                return super.binding(binding);
            }

            @Override
            public Set<CoreExpr> call(Call call) {
                if(call.target.eql(TESTS_KEY))
                    return tests(call.args);
                return super.call(call);
            }
        });
    }

    static public Set<CoreExpr> findExamples(CoreExpr base) {
        return base.acceptVisitor(new TestFindingCoreExprVisitor() {
            @Override
            public Set<CoreExpr> binding(P2<Identifier, CoreExpr> binding) {
                if(binding._1().id.equals(EXAMPLES_KEY.id))
                    return tests(List.single(binding._2()));
                return super.binding(binding);
    		}
    	});
    }

    public static CoreExpr stripScope(CoreExpr x) {
    	while(x instanceof Let) {
    		x = ((Let)x).body;
    	}
    	return x;
    }

}
