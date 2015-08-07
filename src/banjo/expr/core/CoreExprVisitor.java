package banjo.expr.core;

import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.StringLiteral;

public interface CoreExprVisitor<T> {
	T badExpr(BadCoreExpr badExpr);
	T stringLiteral(StringLiteral stringLiteral);
	T numberLiteral(NumberLiteral numberLiteral);
	T identifier(Identifier identifier);
	T call(Call call);
	T objectLiteral(ObjectLiteral objectLiteral);
	T listLiteral(ListLiteral listLiteral);
	T badIdentifier(BadIdentifier badIdentifier);
	T inspect(Inspect inspect);
	T extend(Extend extend);
	T let(Let let);
	T functionLiteral(FunctionLiteral f);
	T baseFunctionRef(BaseFunctionRef baseFunctionRef);
	T projection(Projection projection);
}
