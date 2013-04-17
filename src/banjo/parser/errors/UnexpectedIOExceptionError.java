package banjo.parser.errors;

import java.io.IOException;

public class UnexpectedIOExceptionError extends Error {
	private static final long serialVersionUID = 1L;

	public UnexpectedIOExceptionError(IOException cause) {
		super(cause);
	}
}
