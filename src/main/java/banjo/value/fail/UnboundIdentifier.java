package banjo.value.fail;

import banjo.eval.EvalContext;
import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public class UnboundIdentifier extends Fail {
	public final String id;
    public final Set<SourceFileRange> ranges;
	
    public UnboundIdentifier(EvalContext<?> ctx, Set<SourceFileRange> ranges, String id) {
        super(ctx);
        this.id = id;
        this.ranges = ranges;
    }

    @Override
	public String getMessage() {
        String msg = "The name \"" + id + "\" is not defined / bound here";
        if(!ranges.isEmpty())
            msg = ranges.iterator().next().toString() + ": " + msg;
        return msg;
                
	}
}