package banjo.parser.util;

public class Check {

	/**
	 * This asserts that a value is not null - it acts a sort of annotation on an expression to tell
	 * the null checker the value is not null.
	 *
	 * If the value is in fact, null, this throws a NullPointerException.  Otherwise it returns
	 * the value as-is.
	 */
	public static <T> T nonNull(final T value) {
		assert value != null;
		return value;
	}
}
