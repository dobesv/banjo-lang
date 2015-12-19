package banjo.eval;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class NotCallable extends FailWithSourceFileRangesAndMessage {
	final Object target;

    public NotCallable(Object target, Set<SourceFileRange> ranges) {
        super("Not a function", ranges);
	    this.target = target;
    }

	@Override
	public String getMessage() {
	    return "Not a function: "+target;
	}
}