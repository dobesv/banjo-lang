package banjo.parser.errors;

import banjo.parser.util.FileRange;


public class UnsupportedBinaryOperator extends Problem {
	private static final long serialVersionUID = 1L;

	public UnsupportedBinaryOperator(String op, int offset, int length) {
		super("Unsupported binary operator '"+op+"'", offset, length);
	}

	public UnsupportedBinaryOperator(String op, FileRange range) {
		super("Unsupported binary operator '"+op+"'", range);
	}
}