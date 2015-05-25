package banjo.eval.expr;

import java.util.function.Function;

import fj.P;
import fj.data.TreeMap;

public class TreeMapEnvironment implements Environment {

	public final TreeMap<String, BindingInstance> bindings;
	public final Environment parentEnvironment;

	public TreeMapEnvironment(TreeMap<String, BindingInstance> bindings, Environment parentEnvironment) {
		this.bindings = bindings;
		this.parentEnvironment = parentEnvironment;
	}

	public TreeMapEnvironment(Function<Environment, TreeMap<String, BindingInstance>> bindingsFactory, Environment parentEnvironment) {
		this.parentEnvironment = parentEnvironment;
		this.bindings = bindingsFactory.apply(this);
    }

	@Override
    public BindingInstance apply(String t) {
    	return bindings.get(t).orSome(P.lazy((u) -> parentEnvironment.apply(t)));
    }

}