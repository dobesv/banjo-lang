package banjo.expr.free;

import banjo.eval.UnboundFunctionSelfName;
import banjo.eval.environment.Environment;
import banjo.value.Value;
import fj.P;
import fj.data.Option;

public class FreeBaseFunctionRef implements FreeExpression {
	public final String name;

	public FreeBaseFunctionRef(String name) {
        super();
        this.name = name;
    }

	@Override
	public Value apply(Environment env) {
	    return Option.fromNull(
	    		env.apply(name).baseFunction
	    ).orSome(P.lazy(
	    		(u) -> new UnboundFunctionSelfName("Not a function self-name: '"+name+"'"))
	    );
	}
}