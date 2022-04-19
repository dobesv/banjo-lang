package banjo.value.fail;

import banjo.eval.EvalContext;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class BaseFunctionNotFound extends FailWithSourceFileRangesAndMessage {

    public BaseFunctionNotFound(EvalContext<?> ctx, Set<SourceFileRange> ranges, String id) {
        super(ctx, ranges, Identifier.toSource(id) + " isn't an extension, no base value is available to call");
    }
	
}