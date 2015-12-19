package banjo.eval;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class UnresolvedCodeError extends FailWithSourceFileRangesAndMessage {
	public UnresolvedCodeError(String message,
        Set<SourceFileRange> ranges) {
        super(message, ranges);
    }

}
