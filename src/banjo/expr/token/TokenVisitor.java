package banjo.expr.token;

import banjo.expr.util.FileRange;

public interface TokenVisitor<T> {
	T stringLiteral(FileRange range, String string);
	T numberLiteral(FileRange range, Number number);
	T identifier(FileRange range, String id);
	T operator(FileRange range, String op);
	T whitespace(FileRange range, String text);
	T comment(FileRange range, String text);
	T badToken(FileRange fileRange, String text, String message);
	T eof(FileRange entireFileRange);
}
