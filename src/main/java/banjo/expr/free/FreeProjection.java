package banjo.expr.free;

import banjo.eval.environment.Environment;
import banjo.value.Value;
import fj.data.List;


public class FreeProjection implements FreeExpression {
	public final FreeExpression object;
	public final FreeExpression projection;


	public FreeProjection(FreeExpression object, FreeExpression projection) {
        super();
        this.object = object;
        this.projection = projection;
    }

	@Override
    public Value apply(Environment env, List<Value> trace) {
        Value boundObject = object.apply(env, trace);
        Environment objectAsEnvironment = new Environment(boundObject, env.projectRootObject);
        return projection.apply(objectAsEnvironment, trace);
	}

	@Override
	public String toString() {
	    return object+"."+projection;
	}
}