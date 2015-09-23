package banjo.expr.free;

import banjo.eval.environment.Environment;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.Set;

public class FreeIdentifier implements FreeExpression {
	public final Set<SourceFileRange> ranges;
	public final String id;

	public FreeIdentifier(Set<SourceFileRange> ranges, String id) {
		this.ranges = ranges;
		this.id = id;
    }

	public FreeIdentifier(Identifier id) {
		this(id.getSourceFileRanges(), id.id);
	}
	
	@Override
	public Value apply(Environment t) {
	    return t.getValue(id);
	}

	@Override
	public String toString() {
	    return id;
	}
}