package banjo.dom.core;

import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;

public interface CoreExprVisitor<T> {
	T badExpr(BadCoreExpr badExpr);
	T stringLiteral(StringLiteral stringLiteral);
	T numberLiteral(NumberLiteral numberLiteral);
	T identifier(Identifier identifier);
	T operator(OperatorRef operatorRef);
	T call(Call call);
	T objectLiteral(ObjectLiteral objectLiteral);
	T listLiteral(ListLiteral listLiteral);
	T badIdentifier(BadIdentifier badIdentifier);
	T inspect(Inspect inspect);
	T extend(Extend extend);
	T let(Let let);
	T functionLiteral(FunctionLiteral f);
	T slotReference(SlotReference slotReference);

}
