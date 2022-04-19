package banjo.value.fail;

import banjo.eval.EvalContext;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class UnresolvedCodeError extends FailWithSourceFileRangesAndMessage {
    public UnresolvedCodeError(String message, Set<SourceFileRange> ranges) {
        // We don't pass a backtrace, since code errors don't need runtime
        // context to resolve
        super(EvalContext.NONE, ranges, message);
    }

}
