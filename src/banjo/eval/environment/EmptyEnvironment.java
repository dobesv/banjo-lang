package banjo.eval.environment;

import banjo.eval.UnboundIdentifier;
import banjo.eval.expr.BindingInstance;
import banjo.event.Event;
import banjo.value.Reaction;

public class EmptyEnvironment implements Environment {

	public static final Environment INSTANCE = new EmptyEnvironment();

	@Override
	public BindingInstance apply(String t) {
		return BindingInstance.let(unboundIdentifier(t));
	}

	private UnboundIdentifier unboundIdentifier(String t) {
	    return new UnboundIdentifier("No variable in scope named '"+t+"'");
    }

	@Override
	public String toString() {
	    return "() ⇒";
	}
	
	@Override
	public Reaction<Environment> react(Event event) {
		return Reaction.none(this);
	}
}

