package banjo.idesupport;

import java.util.EnumSet;

public interface SourceTokenVisitor<T> {
	T whitespace(int offset, int length);
	T comment(int offset, int length);
	T endOfFile(int fileLength);
	T operator(int offset, int length);
	T stringLiteral(int offset, int length);
	T numberLiteral(int offset, int length);
	T identifier(int offset, int length, EnumSet<IdentifierFlag> flags);
	T other(int offset, int length);
	T endOfRange(int rangeEnd);
}
