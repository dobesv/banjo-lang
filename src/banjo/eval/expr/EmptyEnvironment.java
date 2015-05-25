package banjo.eval.expr;

import banjo.eval.UnboundIdentifier;

public class EmptyEnvironment implements Environment {

	public static final Environment INSTANCE = new EmptyEnvironment();

	@Override
	public BindingInstance apply(String t) {
		return BindingInstance.let(new UnboundIdentifier("No variable in scope named '"+t+"'"));
	}

}
