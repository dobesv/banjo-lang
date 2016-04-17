package banjo.eval;

import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.List;
import fj.data.Set;

public class UnboundFunctionSelfName extends FailWithSourceFileRangesAndMessage {

    public UnboundFunctionSelfName(List<Value> trace, String message, Set<SourceFileRange> ranges) {
        super(trace, message, ranges);
    }
	
}