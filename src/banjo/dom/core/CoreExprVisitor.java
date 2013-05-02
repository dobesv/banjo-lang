package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;

public interface CoreExprVisitor<T> {
	@Nullable T visitStringLiteral(StringLiteral stringLiteral);
	@Nullable T visitNumberLiteral(NumberLiteral numberLiteral);
	@Nullable T visitIdentifier(Identifier identifier);
	@Nullable T visitOperator(OperatorRef operatorRef);
	@Nullable T visitCall(Call call);
	@Nullable T visitExprList(ExprList exprList);
	@Nullable T visitFieldRef(FieldRef fieldRef);
	@Nullable T visitFunctionLiteral(FunctionLiteral functionLiteral);
	@Nullable T visitObjectLiteral(ObjectLiteral objectLiteral);
	@Nullable T visitLet(Let let);
	@Nullable T visitListLiteral(ListLiteral listLiteral);
	@Nullable T visitSetLiteral(SetLiteral setLiteral);
	@Nullable T visitBadExpr(BadExpr badExpr);
	
}
