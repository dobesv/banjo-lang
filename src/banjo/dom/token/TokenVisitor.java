package banjo.dom.token;

import banjo.parser.util.FileRange;

public interface TokenVisitor<T> {
	T stringLiteral(FileRange range, String string);
	T numberLiteral(FileRange range, Number number, String suffix);
	T identifier(FileRange range, String id);
	T operator(FileRange range, String op);
	T whitespace(FileRange range, String text);
	T comment(FileRange range, String text);
	T badToken(FileRange fileRange, String text, String message);
	T eof(FileRange entireFileRange);
}
