package banjo.expr.free;

import banjo.eval.environment.EmptyEnvironmentRoot;
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
        // TODO A bit of a hack here to make the outermost projection set the
        // project root object in the environment
        Environment objectAsEnvironment =
            env.projectRootObject == EmptyEnvironmentRoot.INSTANCE ? new Environment(boundObject) : new Environment(boundObject, env.projectRootObject);
        return projection.apply(objectAsEnvironment);
	}

	@Override
	public String toString() {
	    return object+"."+projection;
	}
}