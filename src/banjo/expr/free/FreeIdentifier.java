package banjo.expr.free;

import banjo.eval.expr.Environment;
import banjo.expr.util.SourceFileRange;
import fj.data.List;

public class FreeIdentifier implements FreeExpression {
	public final List<SourceFileRange> ranges;
	public final String id;

	public FreeIdentifier(List<SourceFileRange> ranges, String id) {
		this.ranges = ranges;
		this.id = id;
    }

	@Override
	public Object apply(Environment t) {
	    return t.apply(id).value;
	}

	@Override
	public String toString() {
	    return id;
	}
}