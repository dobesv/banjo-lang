package banjo.expr.free;

import banjo.eval.Environment;
import banjo.expr.source.Operator;
import banjo.value.Value;
import fj.Ord;
import fj.P2;
import fj.data.List;
import fj.data.TreeMap;

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
	    return body.apply(env.let(TreeMap.treeMap(Ord.stringOrd, bindings)));
	}

	@Override
	public String toString() {
	    return "(...) "+Operator.LET+" "+body;
	}
}