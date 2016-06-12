package banjo.value.fail;

import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

public class UnboundSlotObject extends FailWithSourceFileRangesAndMessage {

    public UnboundSlotObject(List<?> trace, Set<SourceFileRange> ranges, String id) {
        super(trace, ranges, Identifier.toSource(id) + " is not a slot self-name here");
    }

}