package banjo.eval.coreexpr;

import banjo.dom.source.Operator;
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
	public Object apply(Environment env) {
	    return body.apply(new LetEnvironment(env, bindings));
	}

	@Override
	public String toString() {
	    return "(...) "+Operator.LET+" "+body;
	}
}