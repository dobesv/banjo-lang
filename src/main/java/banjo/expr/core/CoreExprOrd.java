package banjo.expr.core;

import banjo.expr.AbstractBadExpr;
import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;
import fj.F2Functions;
import fj.Ord;
import fj.Ordering;
import fj.data.List;

public class CoreExprOrd {

	public static Ordering _cmp(CoreExpr a1, CoreExpr a2) {
		if (a1 == a2)
			return Ordering.EQ;
		return a1.acceptVisitor(new CoreExprVisitor<Ordering>() {
			@Override
			public Ordering badExpr(BadCoreExpr badExpr) {
				return a2.acceptVisitor(new CoreExprVisitor<Ordering>() {
					@Override
					public Ordering badExpr(BadCoreExpr badExpr2) {
						return AbstractBadExpr.ORD.compare(badExpr, badExpr2);
					}

					@Override
					public Ordering stringLiteral(StringLiteral stringLiteral) {
						return Ordering.LT;
					}

					@Override
					public Ordering numberLiteral(NumberLiteral numberLiteral) {
						return Ordering.LT;
					}

					@Override
					public Ordering identifier(Identifier identifier) {
						return Ordering.LT;
					}

					@Override
					public Ordering listLiteral(ListLiteral listLiteral) {
						return Ordering.LT;
					}

					@Override
					public Ordering badIdentifier(BadIdentifier badIdentifier) {
						return Ordering.LT;
					}

					@Override
					public Ordering extend(Extend extend) {
						return Ordering.LT;
					}

					@Override
					public Ordering scoped(ScopedExpr projection) {
						return Ordering.LT;
					}

					@Override
					public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
						return Ordering.LT;
					}

					@Override
					public Ordering binding(BindingExpr slot) {
						return Ordering.LT;
					}

					@Override
					public Ordering nil() {
						return Ordering.LT;
					}
				});
			}

			@Override
			public Ordering badIdentifier(BadIdentifier badIdentifier) {
				return a2.acceptVisitor(new CoreExprVisitor<Ordering>() {
					@Override
					public Ordering badExpr(BadCoreExpr badExpr2) {
						return Ordering.GT;
					}

					@Override
					public Ordering stringLiteral(StringLiteral stringLiteral) {
						return Ordering.GT;
					}

					@Override
					public Ordering numberLiteral(NumberLiteral numberLiteral) {
						return Ordering.GT;
					}

					@Override
					public Ordering identifier(Identifier identifier) {
						return Ordering.GT;
					}

					@Override
					public Ordering listLiteral(ListLiteral listLiteral) {
						return Ordering.GT;
					}

					@Override
					public Ordering badIdentifier(BadIdentifier badIdentifier2) {
						if (badIdentifier == badIdentifier2)
							return Ordering.EQ;
						return BadIdentifier.ORD.compare(badIdentifier, badIdentifier2);
					}

					@Override
					public Ordering extend(Extend extend) {
						return Ordering.LT;
					}

					@Override
					public Ordering scoped(ScopedExpr projection) {
						return Ordering.LT;
					}

					@Override
					public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
						return Ordering.LT;
					}

					@Override
					public Ordering binding(BindingExpr slot) {
						return Ordering.LT;
					}

					@Override
					public Ordering nil() {
						return Ordering.LT;
					}

				});
			}

			@Override
			public Ordering extend(Extend extend) {
				return a2.acceptVisitor(new CoreExprVisitor<Ordering>() {
					@Override
					public Ordering badExpr(BadCoreExpr badExpr2) {
						return Ordering.GT;
					}

					@Override
					public Ordering stringLiteral(StringLiteral stringLiteral) {
						return Ordering.GT;
					}

					@Override
					public Ordering numberLiteral(NumberLiteral numberLiteral) {
						return Ordering.GT;
					}

					@Override
					public Ordering identifier(Identifier identifier) {
						return Ordering.GT;
					}

					@Override
					public Ordering listLiteral(ListLiteral listLiteral) {
						return Ordering.GT;
					}

					@Override
					public Ordering badIdentifier(BadIdentifier badIdentifier2) {
						return Ordering.GT;
					}

					@Override
					public Ordering extend(Extend extend2) {
						if (extend == extend2)
							return Ordering.EQ;
						return Extend.extendOrd.compare(extend, extend2);
					}

					@Override
					public Ordering scoped(ScopedExpr projection) {
						return Ordering.LT;
					}

					@Override
					public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
						return Ordering.LT;
					}

					@Override
					public Ordering binding(BindingExpr slot) {
						return Ordering.LT;
					}

					@Override
					public Ordering nil() {
						return Ordering.LT;
					}

				});
			}

			@Override
			public Ordering identifier(Identifier identifier) {
				return a2.acceptVisitor(new CoreExprVisitor<Ordering>() {
					@Override
					public Ordering badExpr(BadCoreExpr badExpr2) {
						return Ordering.GT;
					}

					@Override
					public Ordering stringLiteral(StringLiteral stringLiteral) {
						return Ordering.GT;
					}

					@Override
					public Ordering numberLiteral(NumberLiteral numberLiteral) {
						return Ordering.GT;
					}

					@Override
					public Ordering identifier(Identifier identifier2) {
						if (identifier == identifier2)
							return Ordering.EQ;
						return Identifier.ORD.compare(identifier, identifier2);
					}

					@Override
					public Ordering listLiteral(ListLiteral listLiteral) {
						return Ordering.LT;
					}

					@Override
					public Ordering badIdentifier(BadIdentifier badIdentifier2) {
						return Ordering.LT;
					}

					@Override
					public Ordering extend(Extend extend2) {
						return Ordering.LT;
					}

					@Override
					public Ordering scoped(ScopedExpr projection) {
						return Ordering.LT;
					}

					@Override
					public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
						return Ordering.LT;
					}

					@Override
					public Ordering binding(BindingExpr slot) {
						return Ordering.LT;
					}

					@Override
					public Ordering nil() {
						return Ordering.LT;
					}
				});
			}

			@Override
			public Ordering listLiteral(ListLiteral listLiteral) {
				return a2.acceptVisitor(new CoreExprVisitor<Ordering>() {
					@Override
					public Ordering badExpr(BadCoreExpr badExpr2) {
						return Ordering.GT;
					}

					@Override
					public Ordering stringLiteral(StringLiteral stringLiteral) {
						return Ordering.GT;
					}

					@Override
					public Ordering numberLiteral(NumberLiteral numberLiteral) {
						return Ordering.GT;
					}

					@Override
					public Ordering identifier(Identifier identifier2) {
						return Ordering.GT;
					}

					@Override
					public Ordering listLiteral(ListLiteral listLiteral2) {
						if (listLiteral == listLiteral2)
							return Ordering.EQ;
						return ListLiteral.ORD.compare(listLiteral, listLiteral2);
					}

					@Override
					public Ordering badIdentifier(BadIdentifier badIdentifier2) {
						return Ordering.LT;
					}

					@Override
					public Ordering extend(Extend extend2) {
						return Ordering.LT;
					}

					@Override
					public Ordering scoped(ScopedExpr projection) {
						return Ordering.LT;
					}

					@Override
					public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
						return Ordering.LT;
					}

					@Override
					public Ordering binding(BindingExpr slot) {
						return Ordering.LT;
					}

					@Override
					public Ordering nil() {
						return Ordering.LT;
					}

				});
			}

			@Override
			public Ordering numberLiteral(NumberLiteral numberLiteral) {
				return a2.acceptVisitor(new CoreExprVisitor<Ordering>() {
					@Override
					public Ordering badExpr(BadCoreExpr badExpr2) {
						return Ordering.GT;
					}

					@Override
					public Ordering stringLiteral(StringLiteral stringLiteral) {
						return Ordering.GT;
					}

					@Override
					public Ordering numberLiteral(NumberLiteral numberLiteral2) {
						if (numberLiteral == numberLiteral2)
							return Ordering.EQ;
						return NumberLiteral.ORD.compare(numberLiteral, numberLiteral2);
					}

					@Override
					public Ordering identifier(Identifier identifier2) {
						return Ordering.LT;
					}

					@Override
					public Ordering listLiteral(ListLiteral listLiteral2) {
						return Ordering.LT;
					}

					@Override
					public Ordering badIdentifier(BadIdentifier badIdentifier2) {
						return Ordering.LT;
					}

					@Override
					public Ordering extend(Extend extend2) {
						return Ordering.LT;
					}

					@Override
					public Ordering scoped(ScopedExpr projection) {
						return Ordering.LT;
					}

					@Override
					public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
						return Ordering.LT;
					}

					@Override
					public Ordering binding(BindingExpr slot) {
						return Ordering.LT;
					}

					@Override
					public Ordering nil() {
						return Ordering.LT;
					}

				});
			}

			@Override
			public Ordering scoped(ScopedExpr projection1) {
				return a2.acceptVisitor(new CoreExprVisitor<Ordering>() {
					@Override
					public Ordering badExpr(BadCoreExpr badExpr2) {
						return Ordering.GT;
					}

					@Override
					public Ordering stringLiteral(StringLiteral stringLiteral) {
						return Ordering.GT;
					}

					@Override
					public Ordering numberLiteral(NumberLiteral numberLiteral2) {
						return Ordering.GT;
					}

					@Override
					public Ordering identifier(Identifier identifier2) {
						return Ordering.GT;
					}

					@Override
					public Ordering listLiteral(ListLiteral listLiteral2) {
						return Ordering.GT;
					}

					@Override
					public Ordering badIdentifier(BadIdentifier badIdentifier2) {
						return Ordering.GT;
					}

					@Override
					public Ordering extend(Extend extend2) {
						return Ordering.GT;
					}

					@Override
					public Ordering scoped(ScopedExpr projection2) {
						if (projection1 == projection2)
							return Ordering.EQ;
						return ScopedExpr.ORD.compare(projection1, projection2);
					}

					@Override
					public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
						return Ordering.LT;
					}

					@Override
					public Ordering binding(BindingExpr slot) {
						return Ordering.LT;
					}

					@Override
					public Ordering nil() {
						return Ordering.LT;
					}

				});
			}

			@Override
			public Ordering stringLiteral(StringLiteral stringLiteral) {
				return a2.acceptVisitor(new CoreExprVisitor<Ordering>() {
					@Override
					public Ordering badExpr(BadCoreExpr badExpr2) {
						return Ordering.GT;
					}

					@Override
					public Ordering stringLiteral(StringLiteral stringLiteral2) {
						if (stringLiteral == stringLiteral2)
							return Ordering.EQ;
						return StringLiteral.ORD.compare(stringLiteral, stringLiteral2);
					}

					@Override
					public Ordering numberLiteral(NumberLiteral numberLiteral2) {
						return Ordering.LT;
					}

					@Override
					public Ordering identifier(Identifier identifier2) {
						return Ordering.LT;
					}

					@Override
					public Ordering listLiteral(ListLiteral listLiteral2) {
						return Ordering.LT;
					}

					@Override
					public Ordering badIdentifier(BadIdentifier badIdentifier2) {
						return Ordering.LT;
					}

					@Override
					public Ordering extend(Extend extend2) {
						return Ordering.LT;
					}

					@Override
					public Ordering scoped(ScopedExpr projection) {
						return Ordering.LT;
					}

					@Override
					public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject) {
						return Ordering.LT;
					}

					@Override
					public Ordering binding(BindingExpr slot) {
						return Ordering.LT;
					}

					@Override
					public Ordering nil() {
						return Ordering.LT;
					}

				});
			}

			@Override
			public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject1) {
				return a2.acceptVisitor(new CoreExprVisitor<Ordering>() {
					@Override
					public Ordering badExpr(BadCoreExpr badExpr2) {
						return Ordering.GT;
					}

					@Override
					public Ordering stringLiteral(StringLiteral stringLiteral2) {
						return Ordering.GT;
					}

					@Override
					public Ordering numberLiteral(NumberLiteral numberLiteral2) {
						return Ordering.GT;
					}

					@Override
					public Ordering identifier(Identifier identifier2) {
						return Ordering.GT;
					}

					@Override
					public Ordering listLiteral(ListLiteral listLiteral2) {
						return Ordering.GT;
					}

					@Override
					public Ordering badIdentifier(BadIdentifier badIdentifier2) {
						return Ordering.GT;
					}

					@Override
					public Ordering extend(Extend extend2) {
						return Ordering.GT;
					}

					@Override
					public Ordering scoped(ScopedExpr projection) {
						return Ordering.GT;
					}

					@Override
					public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject2) {
						return Ord.intOrd.compare(kernelGlobalObject1.ordinal(), kernelGlobalObject2.ordinal());
					}

					@Override
					public Ordering binding(BindingExpr slot) {
						return Ordering.LT;
					}

					@Override
					public Ordering nil() {
						return Ordering.EQ;
					}
				});
			}


			@Override
			public Ordering binding(BindingExpr slot1) {
				return a2.acceptVisitor(new CoreExprVisitor<Ordering>() {
					@Override
					public Ordering badExpr(BadCoreExpr badExpr2) {
						return Ordering.GT;
					}

					@Override
					public Ordering stringLiteral(StringLiteral stringLiteral2) {
						return Ordering.GT;
					}

					@Override
					public Ordering numberLiteral(NumberLiteral numberLiteral2) {
						return Ordering.GT;
					}

					@Override
					public Ordering identifier(Identifier identifier2) {
						return Ordering.GT;
					}

					@Override
					public Ordering listLiteral(ListLiteral listLiteral2) {
						return Ordering.GT;
					}

					@Override
					public Ordering badIdentifier(BadIdentifier badIdentifier2) {
						return Ordering.GT;
					}

					@Override
					public Ordering extend(Extend extend2) {
						return Ordering.GT;
					}

					@Override
					public Ordering scoped(ScopedExpr projection) {
						return Ordering.GT;
					}

					@Override
					public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject2) {
						return Ordering.GT;
					}

					@Override
					public Ordering binding(BindingExpr slot2) {
						if(slot1 == slot2)
							return Ordering.EQ;
						return BindingExpr.ORD.compare(slot1, slot2);
					}

					@Override
					public Ordering nil() {
						return Ordering.LT;
					}
				});
			}

			@Override
			public Ordering nil() {
				return a2.acceptVisitor(new CoreExprVisitor<Ordering>() {
					@Override
					public Ordering badExpr(BadCoreExpr badExpr2) {
						return Ordering.GT;
					}

					@Override
					public Ordering stringLiteral(StringLiteral stringLiteral2) {
						return Ordering.GT;
					}

					@Override
					public Ordering numberLiteral(NumberLiteral numberLiteral2) {
						return Ordering.GT;
					}

					@Override
					public Ordering identifier(Identifier identifier2) {
						return Ordering.GT;
					}

					@Override
					public Ordering listLiteral(ListLiteral listLiteral2) {
						return Ordering.GT;
					}

					@Override
					public Ordering badIdentifier(BadIdentifier badIdentifier2) {
						return Ordering.GT;
					}

					@Override
					public Ordering extend(Extend extend2) {
						return Ordering.GT;
					}

					@Override
					public Ordering scoped(ScopedExpr projection) {
						return Ordering.GT;
					}

					@Override
					public Ordering kernelGlobalObject(KernelGlobalObject kernelGlobalObject2) {
						return Ordering.GT;
					}

					@Override
					public Ordering binding(BindingExpr slot) {
						return Ordering.GT;
					}
					@Override
					public Ordering nil() {
						return Ordering.EQ;
					}
				});
			}

		});
	}

	/**
	 * Check two CoreExpr instances for equality (ignoring file ranges).
	 * 
	 * @param a First expression
	 * @param b Second expression
	 * @return true if the two expressions are equal
	 */
	public static boolean eql(CoreExpr a, CoreExpr b) {
		return _cmp(a, b) == Ordering.EQ;
	}

	public static final Ord<CoreExpr> ORD = Ord.ord(F2Functions.curry(CoreExprOrd::_cmp));
	public static final Ord<List<CoreExpr>> LIST_ORD = Ord.listOrd(CoreExprOrd.ORD);

}
