package banjo.eval;

import banjo.expr.util.SourceFileRange;
import banjo.value.fail.SlotNotFound;
import fj.data.List;
import fj.data.Set;

public class BaseSlotNotFound<T> extends SlotNotFound<T> {

    public BaseSlotNotFound(List<T> trace, String id, Set<SourceFileRange> ranges, T object) {
        super(trace, id, ranges, object);
	}

    public BaseSlotNotFound(List<T> trace, String id, Set<SourceFileRange> ranges, T object, Throwable cause) {
        super(trace, id, ranges, object, cause);
	}

	@Override
	public String getMessage() {
        return "No base slot '" + id + "' in " + object;
	}

}
