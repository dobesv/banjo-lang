package banjo.parser.util;

import org.eclipse.jdt.annotation.Nullable;

public class Check {

	public static <T> T nonNull(@Nullable T value) {
		if(value == null) throw new NullPointerException();
		return value;
	}
}
