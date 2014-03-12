package banjo.dom.source;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;

public interface AtomVisitor<T> {
	@Nullable T stringLiteral(StringLiteral stringLiteral);
	@Nullable T numberLiteral(NumberLiteral numberLiteral);
	@Nullable T identifier(Identifier identifier);
	@Nullable T operator(OperatorRef operatorRef);

}