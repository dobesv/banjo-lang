package banjo.eval;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class UnboundSlotSelfName extends FailWithSourceFileRangesAndMessage {

    public UnboundSlotSelfName(String message, Set<SourceFileRange> ranges) {
        super(message, ranges);
        // TODO Auto-generated constructor stub
    }

}