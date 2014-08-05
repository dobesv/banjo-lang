package banjo.dom.source;

import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;

public interface AtomVisitor<T> {
	T stringLiteral(StringLiteral stringLiteral);
	T numberLiteral(NumberLiteral numberLiteral);
	T identifier(Identifier identifier);
	T operator(OperatorRef operatorRef);

}