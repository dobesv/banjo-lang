package banjo.dom.token;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;

public interface TokenVisitor<T> {
	@Nullable T stringLiteral(FileRange range, String string);
	@Nullable T numberLiteral(FileRange range, String text, Number number);
	@Nullable T identifier(FileRange range, String id);
	@Nullable T operator(FileRange range, String op);
	@Nullable T whitespace(FileRange range, String text);
	@Nullable T comment(FileRange range, String text);
	@Nullable T badToken(FileRange fileRange, String text, String message);
	@Nullable T eof(FileRange entireFileRange);
}
