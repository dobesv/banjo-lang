package banjo.eval;

import banjo.expr.util.SourceFileRange;
import banjo.value.fail.SlotNotFound;
import fj.data.Set;

public class BaseSlotNotFound<T> extends SlotNotFound<T> {

    public BaseSlotNotFound(EvalContext<T> ctx, String id, Set<SourceFileRange> ranges, T object) {
        super(ctx, id, ranges, object);
	}

    public BaseSlotNotFound(EvalContext<T> ctx, String id, Set<SourceFileRange> ranges, T object, Throwable cause) {
        super(ctx, id, ranges, object, cause);
	}

	@Override
	public String getMessage() {
        return "No base slot '" + id + "' in " + object;
	}

}
