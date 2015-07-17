package banjo.eval.expr;

import banjo.eval.environment.Environment;
import banjo.event.Event;
import banjo.value.Reaction;
import fj.P2;

public class SingleBindingEnvironment implements Environment {
	public final String key;
	public final BindingInstance binding;
	public final Environment parent;


	public SingleBindingEnvironment(String key, BindingInstance binding,
            Environment parent) {
	    super();
	    this.key = key;
	    this.binding = binding;
	    this.parent = parent;
    }

	@Override
	public BindingInstance apply(String t) {
		return t.equals(key) ? binding : parent.apply(t);
	}

	@Override
	public String toString() {
	    return parent + "(" + binding.slotName + " = " + binding.value + ") ⇒ ";
	}

	@Override
	public Reaction<Environment> react(Event event) {
		return Reaction.to(binding, parent, event).map(P2.tuple(this::update));
	}

	public Environment update(BindingInstance newBinding, Environment newParent) {
		if(newBinding == binding && newParent == parent)
			return this;
		return new SingleBindingEnvironment(key, newBinding, newParent);
	}

}
