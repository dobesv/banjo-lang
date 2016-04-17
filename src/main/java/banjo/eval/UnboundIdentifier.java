package banjo.eval;

import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.List;
import fj.data.Set;

public class UnboundIdentifier extends Fail {
	public final String id;
    public final Set<SourceFileRange> ranges;
	
    public UnboundIdentifier(List<Value> trace, String id, Set<SourceFileRange> ranges) {
        super(trace);
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