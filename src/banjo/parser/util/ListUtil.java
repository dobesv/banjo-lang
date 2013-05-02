package banjo.parser.util;

import java.util.ArrayList;
import java.util.List;

public class ListUtil {

	@SafeVarargs
	public static <T> List<T> concat(List<T> a, List<T> ... suffixes) {
		int count=0;
		for(final List<T> b : suffixes) {
			count += b.size();
		}
		final ArrayList<T> result = new ArrayList<T>(count);
		result.addAll(a);
		for(final List<T> b : suffixes) {
			result.addAll(b);
		}
		return result;
	}

}
