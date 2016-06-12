package banjo.value.fail;

import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.List;
import fj.data.Set;

public class BaseFunctionNotFound extends FailWithSourceFileRangesAndMessage {

    public BaseFunctionNotFound(List<Value> trace, Set<SourceFileRange> ranges, String id) {
        super(trace, ranges, Identifier.toSource(id) + " isn't an extension, no base value is available to call");
    }
	
}