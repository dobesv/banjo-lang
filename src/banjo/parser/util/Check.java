package banjo.parser.util;

import org.eclipse.jdt.annotation.DefaultLocation;
import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.annotation.Nullable;

@NonNullByDefault({})
public class Check {

	/**
	 * This asserts that a value is not null - it acts a sort of annotation on an expression to tell
	 * the null checker the value is not null.
	 *
	 * If the value is in fact, null, this throws a NullPointerException.  Otherwise it returns
	 * the value as-is.
	 */
	public static <T> @NonNull T nonNull(final @Nullable T value) {
		assert value != null;
		if(value == null) throw new NullPointerException();
		return value;
	}
}
