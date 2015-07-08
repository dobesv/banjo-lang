package banjo.eval.expr;

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
}
