package banjo.expr.token;

import banjo.expr.util.FileRange;

public interface TokenFold<T> {
	T stringLiteral(T state, FileRange range, int indentColumn, String string);
	T numberLiteral(T state, FileRange range, int indentColumn, Number number);
	T identifier(T state, FileRange range, int indentColumn, String id);
	T operator(T state, FileRange range, int indentColumn, String op);
	T whitespace(T state, FileRange range, String text);
	T comment(T state, FileRange range, String text);
	T badToken(T state, FileRange fileRange, String text, String message);
	T eof(T state, FileRange entireFileRange);

	public static final class RecFold<T extends TokenVisitor<T>> implements TokenFold<T> {

		@Override
		public T stringLiteral(T state, FileRange range, int indentColumn, String string) {
			return state.stringLiteral(range, indentColumn, string);
		}

		@Override
		public T numberLiteral(T state, FileRange range, int indentColumn, Number number) {
			return state.numberLiteral(range, indentColumn, number);
		}

		@Override
		public T identifier(T state, FileRange range, int indentColumn, String id) {
			return state.identifier(range, indentColumn, id);
		}

		@Override
		public T operator(T state, FileRange range, int indentColumn, String op) {
			return state.operator(range, indentColumn, op);
		}

		@Override
		public T whitespace(T state, FileRange range, String text) {
			return state.whitespace(range, text);
		}

		@Override
		public T comment(T state, FileRange range, String text) {
			return state.comment(range, text);
		}

		@Override
		public T badToken(T state, FileRange fileRange, String text,
				String message) {
			return state.badToken(fileRange, text, message);
		}

		@Override
		public T eof(T state, FileRange entireFileRange) {
			return state.eof(entireFileRange);
		}

	}
}
