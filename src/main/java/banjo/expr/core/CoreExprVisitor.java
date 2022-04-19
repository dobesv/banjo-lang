package banjo.expr.core;

import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;

public interface CoreExprVisitor<T> {
    public default T visit(CoreExpr e) {
        return e.acceptVisitor(this);
    }
	T badExpr(BadCoreExpr badExpr);
	T stringLiteral(StringLiteral stringLiteral);
    T numberLiteral(NumberLiteral numberLiteral);
	T identifier(Identifier identifier);
	T listLiteral(ListLiteral listLiteral);
	T badIdentifier(BadIdentifier badIdentifier);
	T extend(Extend extend);
	T scoped(ScopedExpr projection);
	T binding(BindingExpr b);
    T kernelGlobalObject(KernelGlobalObject kernelGlobalObject);
	T nil();
}
