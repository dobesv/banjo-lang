package banjo.parser.util;

import static banjo.parser.util.Check.nonNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ListUtil {

	/**
	 * Concatenate two or more lists, returning a new list.  If only one parameter is provided
	 * the list is simply copied to an ArrayList.  The original list is never modified.
	 */
	@SafeVarargs
	public static <T> List<T> concat(List<T> a, List<T> ... suffixes) {
		if(suffixes.length == 0) {
			if(a.size() == 1) return nonNull(Collections.singletonList(a.get(0)));
			return new ArrayList<T>(a);
		}

		int count=a.size();
		for(final List<T> b : suffixes) {
			count += b.size();
		}
		if(count == 0)
			return nonNull(Collections.<T>emptyList());
		final ArrayList<T> result = new ArrayList<T>(count);
		result.addAll(a);
		for(final List<T> b : suffixes) {
			result.addAll(b);
		}
		return result;
	}

	public static <T> List<T> append(List<T> head, T tail) {
		if(head.isEmpty())
			return nonNull(Collections.singletonList(tail));
		final int count = head.size() + 1;
		final ArrayList<T> result = new ArrayList<>(count);
		result.addAll(head);
		result.add(tail);
		return result;
	}

}
