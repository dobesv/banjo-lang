package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

public interface TokenVisitor<T> extends AtomVisitor<T> {
	@Nullable T visitWhitespace(Whitespace ws);
	@Nullable T visitComment(Comment c);
	@Nullable T visitEof();
}
