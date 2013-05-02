package banjo.dom.source;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.token.Ellipsis;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.dom.token.UnitRef;

public interface AtomVisitor<T> {
	@Nullable T visitStringLiteral(StringLiteral stringLiteral);
	@Nullable T visitNumberLiteral(NumberLiteral numberLiteral);
	@Nullable T visitEllipsis(Ellipsis ellipsis);
	@Nullable T visitUnit(UnitRef unit);
	@Nullable T visitIdentifier(Identifier identifier);
	@Nullable T visitOperator(OperatorRef operatorRef);
	
}