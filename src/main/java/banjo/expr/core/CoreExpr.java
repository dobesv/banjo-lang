package banjo.expr.core;

import banjo.expr.AbstractBadExpr;
import banjo.expr.Expr;
import banjo.expr.source.SourceExpr;
import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;
import fj.F2Functions;
import fj.Ord;
import fj.Ordering;
import fj.data.List;
import fj.data.Set;

/**
 * Core expressions are the ones that may be emitted by the desugaring process.
 */
public interface CoreExpr extends Expr {


	public static Ordering _cmp(CoreExpr a1, CoreExpr a2) {
		if(a1 == a2) return Ordering.EQ;
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
                    public Ordering call(Call call) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteral objectLiteral) {
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
                    public Ordering let(Let let) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering functionLiteral(FunctionLiteral f) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering baseFunctionRef(BaseFunctionRef baseFunctionRef) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering projection(Projection projection) {
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
                    public Ordering call(Call call) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteral objectLiteral) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering listLiteral(ListLiteral listLiteral) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering badIdentifier(BadIdentifier badIdentifier2) {
                        return BadIdentifier.ORD.compare(badIdentifier, badIdentifier2);
                    }

                    @Override
                    public Ordering extend(Extend extend) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering let(Let let) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering functionLiteral(FunctionLiteral f) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering baseFunctionRef(BaseFunctionRef baseFunctionRef) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering projection(Projection projection) {
                        return Ordering.LT;
                    }
                });
			}
			@Override
			public Ordering call(Call call) {
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
                    public Ordering call(Call call2) {
                        return Call.callOrd.compare(call, call2);
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteral objectLiteral) {
                        return Ordering.LT;
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
                    public Ordering extend(Extend extend) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering let(Let let) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering functionLiteral(FunctionLiteral f) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering baseFunctionRef(BaseFunctionRef baseFunctionRef) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering projection(Projection projection) {
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
                    public Ordering call(Call call2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteral objectLiteral) {
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
                        return Extend.extendOrd.compare(extend, extend2);
                    }

                    @Override
                    public Ordering let(Let let) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering functionLiteral(FunctionLiteral f) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering baseFunctionRef(BaseFunctionRef baseFunctionRef) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering projection(Projection projection) {
                        return Ordering.LT;
                    }
                });
			}

			@Override
			public Ordering functionLiteral(FunctionLiteral f) {
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
                    public Ordering call(Call call2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteral objectLiteral) {
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
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering let(Let let) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering functionLiteral(FunctionLiteral f2) {
                        return FunctionLiteral.functionLiteralOrd.compare(f, f2);
                    }

                    @Override
                    public Ordering baseFunctionRef(BaseFunctionRef baseFunctionRef) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering projection(Projection projection) {
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
                        return Identifier.ORD.compare(identifier, identifier2);
                    }

                    @Override
                    public Ordering call(Call call2) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteral objectLiteral) {
                        return Ordering.LT;
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
                    public Ordering let(Let let) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering functionLiteral(FunctionLiteral f2) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering baseFunctionRef(BaseFunctionRef baseFunctionRef) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering projection(Projection projection) {
                        return Ordering.LT;
                    }
                });
			}

			@Override
			public Ordering let(Let let) {
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
                    public Ordering call(Call call2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteral objectLiteral) {
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
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering let(Let let2) {
                        return Let.ORD.compare(let, let2);
                    }

                    @Override
                    public Ordering functionLiteral(FunctionLiteral f2) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering baseFunctionRef(BaseFunctionRef baseFunctionRef) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering projection(Projection projection) {
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
                    public Ordering call(Call call2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteral objectLiteral) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering listLiteral(ListLiteral listLiteral2) {
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
                    public Ordering let(Let let2) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering functionLiteral(FunctionLiteral f2) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering baseFunctionRef(BaseFunctionRef baseFunctionRef) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering projection(Projection projection) {
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
                        return NumberLiteral.ORD.compare(numberLiteral, numberLiteral2);
                    }

                    @Override
                    public Ordering identifier(Identifier identifier2) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering call(Call call2) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteral objectLiteral) {
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
                    public Ordering let(Let let2) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering functionLiteral(FunctionLiteral f2) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering baseFunctionRef(BaseFunctionRef baseFunctionRef) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering projection(Projection projection) {
                        return Ordering.LT;
                    }
                });
			}

			@Override
			public Ordering objectLiteral(ObjectLiteral objectLiteral) {
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
                    public Ordering call(Call call2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteral objectLiteral2) {
                        return ObjectLiteral.ORD.compare(objectLiteral, objectLiteral2);
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
                    public Ordering let(Let let2) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering functionLiteral(FunctionLiteral f2) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering baseFunctionRef(BaseFunctionRef baseFunctionRef) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering projection(Projection projection) {
                        return Ordering.LT;
                    }
                });
			}

			@Override
			public Ordering projection(Projection slotReference) {
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
                    public Ordering call(Call call2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteral objectLiteral2) {
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
                    public Ordering let(Let let2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering functionLiteral(FunctionLiteral f2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering baseFunctionRef(BaseFunctionRef baseFunctionRef) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering projection(Projection projection2) {
                        return Projection.ORD.compare(slotReference, projection2);
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
                    public Ordering call(Call call2) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteral objectLiteral) {
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
                    public Ordering let(Let let2) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering functionLiteral(FunctionLiteral f2) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering baseFunctionRef(BaseFunctionRef baseFunctionRef) {
                        return Ordering.LT;
                    }

                    @Override
                    public Ordering projection(Projection projection) {
                        return Ordering.LT;
                    }
                });
            }
			@Override
			public Ordering baseFunctionRef(BaseFunctionRef baseFunctionRef) {
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
                    public Ordering call(Call call2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering objectLiteral(ObjectLiteral objectLiteral) {
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
                    public Ordering let(Let let2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering functionLiteral(FunctionLiteral f2) {
                        return Ordering.GT;
                    }

                    @Override
                    public Ordering baseFunctionRef(BaseFunctionRef baseFunctionRef2) {
                        return BaseFunctionRef.ORD.compare(baseFunctionRef, baseFunctionRef2);
                    }

                    @Override
                    public Ordering projection(Projection projection) {
                        return Ordering.LT;
                    }
                });
			}
		});
	}


	/**
	 * Visitor pattern
	 */
    <T> T acceptVisitor(CoreExprVisitor<T> visitor);

	/**
	 * Object algebra pattern (kind of a bottom-up visitor pattern)
	 */
	<T> T acceptVisitor(CoreExprAlgebra<T> visitor);

    public static final Ord<CoreExpr> coreExprOrd = Ord.ord(F2Functions.curry(CoreExpr::_cmp));
	public static final Ord<List<CoreExpr>> listOfCoreExprOrd = Ord.listOrd(CoreExpr.coreExprOrd);
    public static final Set<CoreExpr> EMPTY_SET = Set.empty(coreExprOrd);

	/**
	 * Parse a string to a CoreExpr
	 */
	public static CoreExpr fromString(String src) {
		final SourceExpr parseTree = SourceExpr.fromString(src);
		return fromSourceExpr(parseTree);
	}

    /**
     * Parse a SourceExpr syntax tree into a CoreExpr AST.
     */
	public static CoreExpr fromSourceExpr(final SourceExpr parseTree) {
	    return new CoreExprFactory().desugar(parseTree).getValue();
    }

	/**
	 * True if this CoreExpr is the same as the other, ignoring source file location.
	 */
	default boolean eql(CoreExpr other) {
		return coreExprOrd.eq(this, other);
	}

}
