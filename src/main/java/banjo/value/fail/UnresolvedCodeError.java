package banjo.value.fail;

import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

public class UnresolvedCodeError extends FailWithSourceFileRangesAndMessage {
    public UnresolvedCodeError(String message, Set<SourceFileRange> ranges) {
        // We don't pass a backtrace, since code errors don't need runtime
        // context to resolve
        super(List.nil(), ranges, message);
    }

}
