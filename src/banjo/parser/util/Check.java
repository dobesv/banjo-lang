package banjo.parser.util;

import org.eclipse.jdt.annotation.Nullable;

public class Check {

	/**
	 * This ensures a value is not null - it acts a sort of annotation on an expression to tell
	 * the null checker the value is not null.
	 * 
	 * If the value is in fact, null, this throws a NullPointerException.  Otherwise it returns
	 * the value as-is.
	 */
	public static <T> T nonNull(@Nullable T value) {
		if(value == null) throw new NullPointerException();
		return value;
	}
}
