package banjo.expr.free;

import banjo.eval.Environment;
import banjo.value.SlotValue;
import banjo.value.Value;


public class FreeProjection implements FreeExpression {
	public final FreeExpression object;
	public final FreeExpression projection;


	public FreeProjection(FreeExpression object, FreeExpression projection) {
        super();
        this.object = object;
        this.projection = projection;
    }

	@Override
	public Value apply(Environment env) {
		return projection.apply(new Environment(object.apply(env)));
	}

	@Override
	public String toString() {
	    return object+"."+projection;
	}
}