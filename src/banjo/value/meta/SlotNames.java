package banjo.value.meta;

import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.Set;

/**
 * Special value that for any slot returns the name of that slot as the result.
 * <p>
 * This requires a function that wraps a java native string into something that
 * implements the API expected by banjo code.
 */
public class SlotNames implements Value {
    private final Value stringWrapper;

    /**
     * Construct an instance of the slot name generator.
     * <p>
     * This requires a function that wraps a java native string into something
     * that implements the API expected by banjo code.
     * 
     * @param stringWrapper
     *            Banjo function to wrap the string into a banjo string
     */
	public SlotNames(Value stringWrapper) {
        super();
        this.stringWrapper = stringWrapper;
    }

	@Override
	public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
        return stringWrapper.call1(Value.fromJava(name));
	}
	
	@Override
	public Value slot(String name, Set<SourceFileRange> ranges) {
        return stringWrapper.call1(Value.fromJava(name));
	}
}
