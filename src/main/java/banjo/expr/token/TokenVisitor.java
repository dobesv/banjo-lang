package banjo.expr.token;

import banjo.expr.util.FileRange;

public interface TokenVisitor<T> {
	T stringLiteral(FileRange range, int indentColumn, String string, boolean kernelString);
    T numberLiteral(FileRange range, int indentColumn, Number number, String source, boolean kernelNumber);
	T identifier(FileRange range, int indentColumn, String id);
	T operator(FileRange range, int indentColumn, String op);
	T whitespace(FileRange range, String text);
	T comment(FileRange range, String text);
	T badToken(FileRange fileRange, String text, String message);
	T eof(FileRange entireFileRange);
}
