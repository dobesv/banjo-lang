package banjo.eval.environment;

import java.util.function.Function;

import banjo.eval.expr.BindingInstance;
import banjo.event.Event;
import banjo.value.Reaction;
import fj.Ord;
import fj.P2;
import fj.data.List;
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
	
	@Override
	public Reaction<Environment> react(Event event) {
		List<P2<String, BindingInstance>> pairs = List.iterableList(bindings);
		List<BindingInstance> deps = pairs.map(P2.__2());
		Reaction<List<BindingInstance>> reactions = Reaction.to(deps, event);
		boolean changedSlots = reactions.v != deps;
		TreeMap<String, BindingInstance> newSlots = 
				changedSlots ? TreeMap.treeMap(Ord.stringOrd, pairs.map(P2.__1()).zip(reactions.v)) :
				this.bindings;
		return Reaction.p(reactions.from(newSlots), parentEnvironment.react(event)).map(P2.tuple(this::update));
	}

	public Environment update(TreeMap<String, BindingInstance> newBindings, Environment newParentEnvironment) {
		if(newBindings == bindings && newParentEnvironment == parentEnvironment)
			return this;
		return new TreeMapEnvironment(newBindings, newParentEnvironment);
	}
}