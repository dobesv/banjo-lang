package banjo.eval;

import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.List;
import fj.data.Set;

public class NotCallable extends FailWithSourceFileRangesAndMessage {
	final Object target;

    public NotCallable(List<Value> trace, Object target, Set<SourceFileRange> ranges) {
        super(trace, "Not a function", ranges);
	    this.target = target;
    }

	@Override
	public String getMessage() {
	    return "Not a function: "+target;
	}
}