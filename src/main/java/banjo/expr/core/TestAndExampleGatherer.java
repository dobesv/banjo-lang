package banjo.expr.core;

import banjo.expr.token.Identifier;
import fj.P2;
import fj.data.List;
import fj.data.Set;

public class TestAndExampleGatherer {

	private static class TestFindingCoreExprVisitor extends BaseCoreExprVisitor<Set<CoreExpr>> {
		@Override
		public Set<CoreExpr> fallback() {
			return CoreExpr.EMPTY_SET;
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
		public Set<CoreExpr> listLiteral(ListLiteral n) {
			List<Set<CoreExpr>> eltTests = n.elements.map(this::visit);
			return union(eltTests);

		}

		public Set<CoreExpr> binding(P2<Identifier, CoreExpr> binding) {
			return binding._2().acceptVisitor(this);
		}

		@Override
		public Set<CoreExpr> extend(Extend n) {
			return n.getBase().acceptVisitor(this).union(n.getExtension().acceptVisitor(this));
		}

		@Override
		public Set<CoreExpr> scoped(ScopedExpr projection) {
			return union(projection.getArgs().map(this::visit));
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
				if (binding._1().id.equals(TESTS_KEY.id))
					return tests(List.single(binding._2()));
				return super.binding(binding);
			}
		});
	}

	static public Set<CoreExpr> findExamples(CoreExpr base) {
		return base.acceptVisitor(new TestFindingCoreExprVisitor() {
			@Override
			public Set<CoreExpr> binding(P2<Identifier, CoreExpr> binding) {
				if (binding._1().id.equals(EXAMPLES_KEY.id))
					return tests(List.single(binding._2()));
				return super.binding(binding);
			}
		});
	}

	public static CoreExpr stripScope(CoreExpr x) {
		return x.acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {
			@Override
			public CoreExpr scoped(ScopedExpr projection) {
				if (projection.isExtendingCurrentScope()) {
					return projection.getBody().acceptVisitor(this);
				}
				return fallback();
			}

			@Override
			public CoreExpr fallback() {
				return x;
			}
		});
	}

}
