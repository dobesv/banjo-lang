package banjo.expr.free;

import banjo.eval.environment.Environment;
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
		Value boundObject = object.apply(env);
        Environment objectAsEnvironment = new Environment(boundObject, env.rootEnvironment);
        return projection.apply(objectAsEnvironment);
	}

	@Override
	public String toString() {
	    return object+"."+projection;
	}
}