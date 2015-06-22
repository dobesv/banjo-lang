package banjo.eval.input;

import fj.Ord;
import fj.data.Set;

public interface InputValue extends Comparable<InputValue> {

	public static final Ord<InputValue> ord = Ord.comparableOrd();
	public static final Set<InputValue> emptySet = Set.empty(ord);
	default public long getNextPollTime() { return Long.MAX_VALUE; }
	public Object currentValue();
}
