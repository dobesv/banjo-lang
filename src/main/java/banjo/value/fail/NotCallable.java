package banjo.value.fail;

import banjo.eval.EvalContext;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class NotCallable extends FailWithSourceFileRangesAndMessage {
	final Object target;

    public NotCallable(EvalContext<?> ctx, Object target, Set<SourceFileRange> ranges) {
        super(ctx, ranges, "Not a function");
	    this.target = target;
    }

	@Override
	public String getMessage() {
	    return "Not a function: "+target;
	}
}