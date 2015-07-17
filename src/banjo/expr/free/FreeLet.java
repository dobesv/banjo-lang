package banjo.expr.free;

import banjo.eval.environment.Environment;
import banjo.eval.environment.LetEnvironment;
import banjo.expr.source.Operator;
import banjo.value.Value;
import fj.P2;
import fj.data.List;

public class FreeLet implements FreeExpression {
	public final List<P2<String, FreeExpression>> bindings;
	public final FreeExpression body;
	public FreeLet(List<P2<String, FreeExpression>> bindings,
            FreeExpression body) {
        super();
        this.bindings = bindings;
        this.body = body;
    }
	@Override
	public Value apply(Environment env) {
	    return body.apply(new LetEnvironment(env, bindings));
	}

	@Override
	public String toString() {
	    return "(...) "+Operator.LET+" "+body;
	}
}