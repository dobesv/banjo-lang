package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;

public interface TokenVisitor<T> {
	@Nullable T visitStringLiteral(FileRange range, StringLiteral stringLiteral);
	@Nullable T visitNumberLiteral(FileRange range, NumberLiteral numberLiteral);
	@Nullable T visitEllipsis(FileRange range, Ellipsis ellipsis);
	@Nullable T visitUnit(FileRange range, UnitRef unit);
	@Nullable T visitIdentifier(FileRange range, Identifier identifier);
	@Nullable T visitOperator(FileRange range, OperatorRef operatorRef);
	@Nullable T visitWhitespace(FileRange range, Whitespace ws);
	@Nullable T visitComment(FileRange range, Comment c);
	@Nullable T visitEof(FileRange entireFileRange);
}
