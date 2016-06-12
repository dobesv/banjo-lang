package banjo.eval.expr;

import banjo.eval.resolver.InstanceAlgebra;
import fj.data.List;

/**
 * A closed slot instance is not affected by extension operations because it
 * only depends on its lexical environment.
 */
public class ClosedSlotInstance<T> implements SlotInstance<T> {

    public final T value;

    public ClosedSlotInstance(T value) {
        super();
        this.value = value;
    }

    @Override
    public T apply(List<T> trace, T t, T u, InstanceAlgebra<T> a) {
        return this.value;
	}
	
	@Override
	public String toString() {
        return value.toString();
	}

    @Override
    public <V> V acceptVisitor(SlotInstanceVisitor<T, V> visitor) {
        return visitor.closed(this);
    }

}
