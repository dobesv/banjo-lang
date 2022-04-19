package banjo.value.special;

import banjo.eval.EvalContext;
import banjo.eval.util.Selector;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

/**
 * Catch all slot accesses to this object and give them to a function which can
 * send the access to whatever other object(s) it wants.
 * <p>
 * For example a slot access to "x.foo" could become a function "tmp -> tmp.foo"
 * which is passed to the function provided as the interceptor. That function
 * can then read the given slot on whatever objects it wants to.
 * <p>
 * Alternatively, the function could ignore its parameter and just return a
 * constant for all slots.
 */
public class DynamicSlotProxy implements Value {
	public final Value delegate;
	
	public DynamicSlotProxy(Value delegate) {
		this.delegate = delegate;
    }

	@Override
    public Value slot(EvalContext<Value> ctx, Value self, String name, Set<SourceFileRange> ranges, Option<Value> fallback) {
        final Value sel = new Selector(ctx, name, ranges);
        return delegate.call(ctx, List.list(sel).append(fallback.toList()));
	}

	public DynamicSlotProxy update(Value newInterceptor) {
		if(newInterceptor == delegate)
			return this;
		return new DynamicSlotProxy(newInterceptor);
	}

	@Override
	public boolean isDefined(EvalContext<Value> ctx) {
		return delegate.isDefined(ctx);
	}
	
    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.dynamicSlotProxy(this);
    }
}
