package banjo.expr.core;

import banjo.expr.AbstractBadExpr;
import banjo.expr.Expr;
import banjo.expr.source.SourceExpr;
import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.OrdUtil;
import fj.F2Functions;
import fj.Ord;
import fj.Ordering;
import fj.data.List;

/**
 * Core expressions are the ones that may be emitted by the desugaring process.
 */
public interface CoreExpr extends Expr {


	public static Ordering _cmp(CoreExpr a1, CoreExpr a2) {
		if(a1 == a2) return Ordering.EQ;
		return a1.acceptVisitor(new CoreExprVisitor<Ordering>() {
			@Override
			public Ordering badExpr(BadCoreExpr badExpr) {
			    return AbstractBadExpr.ORD.compare(badExpr, (BadCoreExpr)a2);
			}
			@Override
			public Ordering badIdentifier(BadIdentifier badIdentifier) {
			    return BadIdentifier.ORD.compare(badIdentifier, (BadIdentifier)a2);
			}
			@Override
			public Ordering call(Call call) {
			    return Call.callOrd.compare(call,  (Call)a2);
			}
			@Override
			public Ordering extend(Extend extend) {
			    return Extend.extendOrd.compare(extend, (Extend)a2);
			}

			@Override
			public Ordering functionLiteral(FunctionLiteral f) {
			    return FunctionLiteral.functionLiteralOrd.compare(f, (FunctionLiteral)a2);
			}

			@Override
			public Ordering identifier(Identifier identifier) {
			    return Identifier.ORD.compare(identifier, (Identifier)a2);
			}

			@Override
			public Ordering inspect(Inspect inspect) {
			    return Inspect.ORD.compare(inspect, (Inspect)a2);
			}

			@Override
			public Ordering let(Let let) {
			    return Let.ORD.compare(let, (Let)a2);
			}

			@Override
			public Ordering listLiteral(ListLiteral listLiteral) {
			    return ListLiteral.ORD.compare(listLiteral, (ListLiteral)a2);
			}

			@Override
			public Ordering numberLiteral(NumberLiteral numberLiteral) {
			    return NumberLiteral.ORD.compare(numberLiteral, (NumberLiteral)a2);
			}

			@Override
			public Ordering objectLiteral(ObjectLiteral objectLiteral) {
			    return ObjectLiteral.ORD.compare(objectLiteral, (ObjectLiteral)a2);
			}

			@Override
			public Ordering projection(Projection slotReference) {
			    return Projection.ORD.compare(slotReference, (Projection)a2);
			}
			@Override
            public Ordering stringLiteral(StringLiteral stringLiteral) {
                return StringLiteral.ORD.compare(stringLiteral, (StringLiteral)a2);
            }
			@Override
			public Ordering baseFunctionRef(BaseFunctionRef baseFunctionRef) {
			    return BaseFunctionRef.ORD.compare(baseFunctionRef, (BaseFunctionRef)a2);
			}
		});
	}

	public static final Ord<CoreExpr> _coreExprsOfSameClassOrd = Ord.ord(F2Functions.curry(CoreExpr::_cmp));


	/**
	 * Visitor pattern
	 */
    <T> T acceptVisitor(CoreExprVisitor<T> visitor);

	/**
	 * Object algebra pattern (kind of a bottom-up visitor pattern)
	 */
	<T> T acceptVisitor(CoreExprAlgebra<T> visitor);

	public static final Ord<CoreExpr> coreExprOrd = OrdUtil.chain(CLASS_NAME_ORD, _coreExprsOfSameClassOrd);
	public static final Ord<List<CoreExpr>> listOfCoreExprOrd = Ord.listOrd(CoreExpr.coreExprOrd);

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
