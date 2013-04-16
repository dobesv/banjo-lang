package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

public interface AtomVisitor<T> {
	@Nullable T visitStringLiteral(StringLiteral stringLiteral);
	@Nullable T visitNumberLiteral(NumberLiteral numberLiteral);
	@Nullable T visitEllipsis(Ellipsis ellipsis);
	@Nullable T visitUnit(UnitRef unit);
	@Nullable T visitIdentifier(Identifier identifier);
	@Nullable T visitOperator(OperatorRef operatorRef);
	
}