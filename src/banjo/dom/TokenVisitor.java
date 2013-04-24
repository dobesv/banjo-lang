package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;

public interface TokenVisitor<T> extends AtomVisitor<T> {
	@Nullable T visitWhitespace(Whitespace ws);
	@Nullable T visitComment(Comment c);
	@Nullable T visitEof(FileRange entireFileRange);
}
