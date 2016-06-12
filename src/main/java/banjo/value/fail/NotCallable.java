package banjo.value.fail;

import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

public class NotCallable extends FailWithSourceFileRangesAndMessage {
	final Object target;

    public NotCallable(List<?> trace, Object target, Set<SourceFileRange> ranges) {
        super(trace, ranges, "Not a function");
	    this.target = target;
    }

	@Override
	public String getMessage() {
	    return "Not a function: "+target;
	}
}