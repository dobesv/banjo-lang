package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.token.BadIdentifier;
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
	@Nullable T objectLiteral(ObjectLiteral objectLiteral);
	@Nullable T listLiteral(ListLiteral listLiteral);
	@Nullable T badExpr(BadCoreExpr badExpr);
	@Nullable T badIdentifier(BadIdentifier badIdentifier);

}
