package banjo.expr.free;

import banjo.eval.environment.Environment;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.List;

public class FreeIdentifier implements FreeExpression {
	public final List<SourceFileRange> ranges;
	public final String id;

	public FreeIdentifier(List<SourceFileRange> ranges, String id) {
		this.ranges = ranges;
		this.id = id;
    }

	@Override
	public Value apply(Environment t) {
	    return t.apply(id).value;
	}

	@Override
	public String toString() {
	    return id;
	}
}