package banjo.value.fail;

import banjo.eval.EvalContext;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class UnboundFunctionCallee extends FailWithSourceFileRangesAndMessage {

    public UnboundFunctionCallee(EvalContext<?> ctx, Set<SourceFileRange> ranges, String id) {
        super(ctx, ranges, Identifier.toSource(id) + " is not a function self-name here");
    }
	
}