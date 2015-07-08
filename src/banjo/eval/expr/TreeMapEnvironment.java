package banjo.eval.expr;

import java.util.function.Function;

import fj.P;
import fj.data.Option;
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
    	final Option<BindingInstance> binding = bindings.get(t);
    	if(binding.isSome())
    		return binding.some();
		return parentEnvironment.apply(t);
    }

	@Override
	public String toString() {
	    return parentEnvironment + "("+bindings+") ⇒ ";
	}
}