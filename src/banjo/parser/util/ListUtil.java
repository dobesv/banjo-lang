package banjo.parser.util;

import static banjo.parser.util.Check.nonNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

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

	public static <T> List<T> prepend(T head, List<T> tail) {
		if(tail.isEmpty())
			return nonNull(Collections.<T>singletonList(head));
		final int count = tail.size() + 1;
		final ArrayList<T> result = new ArrayList<>(count);
		result.add(head);
		result.addAll(tail);
		return result;
	}

	public static <T extends Comparable<T>> int compare(@Nullable Iterable<? extends T> list1, @Nullable Iterable<? extends T> list2) {
		if(list1 == null) return (list2 == null ? 0 : 1);
		if(list2 == null) return -1;
		final Iterator<? extends T> it1 = list1.iterator(), it2 = list2.iterator();
		while(it1.hasNext() && it2.hasNext()) {
			final int cmp = it1.next().compareTo(it2.next());
			if(cmp != 0) return cmp;
		}
		return Boolean.compare(it1.hasNext(), it2.hasNext());
	}

}
