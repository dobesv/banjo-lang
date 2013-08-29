package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;

public interface TokenVisitor<T> {
	@Nullable T stringLiteral(FileRange range, StringLiteral stringLiteral);
	@Nullable T numberLiteral(FileRange range, NumberLiteral numberLiteral);
	@Nullable T ellipsis(FileRange range, Ellipsis ellipsis);
	@Nullable T identifier(FileRange range, Identifier identifier);
	@Nullable T operator(FileRange range, OperatorRef operatorRef);
	@Nullable T whitespace(FileRange range, Whitespace ws);
	@Nullable T comment(FileRange range, Comment c);
	@Nullable T badToken(FileRange fileRange, BadToken badToken);
	@Nullable T eof(FileRange entireFileRange);
}
