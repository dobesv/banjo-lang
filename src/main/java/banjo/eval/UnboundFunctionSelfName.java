package banjo.eval;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class UnboundFunctionSelfName extends FailWithSourceFileRangesAndMessage {

    public UnboundFunctionSelfName(String message, Set<SourceFileRange> ranges) {
        super(message, ranges);
    }
	
}