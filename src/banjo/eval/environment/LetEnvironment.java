package banjo.eval.environment;

import banjo.eval.expr.BindingInstance;
import banjo.expr.free.FreeExpression;
import banjo.value.Value;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.TreeMap;

public class LetEnvironment extends TreeMapEnvironment {
	public LetEnvironment(Environment parentEnvironment, List<P2<String, FreeExpression>> bindings) {
		super(e -> bind(bindings, e), parentEnvironment);
    }

	private static TreeMap<String, BindingInstance> bind(List<P2<String, FreeExpression>> bindings, Environment recursiveEnv) {
	    return TreeMap.treeMap(Ord.stringOrd, bindings.map(p -> P.p(p._1(), bindOne(p._2(), recursiveEnv))));
    }

	private static BindingInstance bindOne(FreeExpression f, Environment recursiveEnv) {
	    return BindingInstance.let(Value.lazy(() -> f.apply(recursiveEnv)));
    }
}