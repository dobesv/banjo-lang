package banjo.value.fail;

import banjo.eval.EvalContext;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class UnboundSlotObject<T> extends FailWithSourceFileRangesAndMessage {

    public UnboundSlotObject(EvalContext<T> ctx, Set<SourceFileRange> ranges, String id) {
        super(ctx, ranges, Identifier.toSource(id) + " is not a slot self-name here");
    }

}