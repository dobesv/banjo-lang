package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;

public interface CoreExprVisitor<T> {
	@Nullable T stringLiteral(StringLiteral stringLiteral);
	@Nullable T numberLiteral(NumberLiteral numberLiteral);
	@Nullable T identifier(Identifier identifier);
	@Nullable T operator(OperatorRef operatorRef);
	@Nullable T call(Call call);
	@Nullable T exprPair(ExprPair exprList);
	@Nullable T projection(Projection projection);
	@Nullable T functionLiteral(FunctionLiteral functionLiteral);
	@Nullable T objectLiteral(ObjectLiteral objectLiteral);
	@Nullable T let(Let let);
	@Nullable T listLiteral(ListLiteral listLiteral);
	@Nullable T setLiteral(SetLiteral setLiteral);
	@Nullable T badExpr(BadExpr badExpr);
	
}
