package banjo.eval.coreexpr;

import banjo.parser.util.SourceFileRange;
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