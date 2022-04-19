package banjo.value;

import banjo.eval.EvalContext;
import banjo.value.fail.ArgumentNotSupplied;
import fj.data.List;

/**
 * A tuple captures an argument list so it can be passed around.
 */
public class TupleValue implements Value {
    public final List<Value> arguments;

    public TupleValue(List<Value> arguments) {
        this.arguments = arguments;
    }

    @Override
    public Value call(EvalContext<Value> ctx, List<Value> arguments) {
        if (arguments.isEmpty())
            return new ArgumentNotSupplied(ctx, "target");
        Value callee = arguments.head();
        return callee.call(ctx, this.arguments);
    }

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.tuple(this);
    }
}
