package banjo.expr.source;

import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.OperatorRef;
import banjo.expr.token.StringLiteral;

public interface AtomVisitor<T> {
	T stringLiteral(StringLiteral stringLiteral);

    T numberLiteral(NumberLiteral numberLiteral);

	T identifier(Identifier identifier);

	T operator(OperatorRef operatorRef);

}