package banjo.eval;

import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import banjo.value.fail.Fail;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

/**
 * Wrap an object so that if it is extended or used to extend another object, it
 * won't use slots from it's new base or extension in its calculation.
 */
public class ClosedObject implements Value {
    final Value object;

    @Override
    public Value call(EvalContext<Value> ctx, Value recurse, Value baseImpl, List<Value> arguments) {
        return object.call(ctx, arguments);
    }

    @Override
    public Value call(EvalContext<Value> ctx, List<Value> arguments) {
        return object.call(ctx, arguments);
    }

    @Override
    public Value call1(EvalContext<Value> ctx, Value v) {
        return object.call1(ctx, v);
    }

    @Override
    public Value slot(EvalContext<Value> ctx, Value self, String name, Set<SourceFileRange> ranges, Option<Value> fallback) {
        return slot(ctx, name, ranges);
    }

    @Override
    public Value slot(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges) {
        return object.slot(ctx, name, ranges);
    }

    @Override
    public Value callMethod(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
        return object.callMethod(ctx, name, ranges, args);
    }

    @Override
    public Value callMethod(EvalContext<Value> ctx, String name, Set<SourceFileRange> ranges, List<Value> args) {
        return object.callMethod(ctx, name, ranges, args);
    }

    @Override
    public Value force(EvalContext<Value> ctx) {
        return object.force(ctx);
    }

    @Override
    public boolean isDefined(EvalContext<Value> ctx) {
        return object.isDefined(ctx);
    }

    @Override
    public <T> Either<T, Fail> convertToJava(EvalContext<Value> ctx, Class<T> clazz) {
        return object.convertToJava(ctx, clazz);
    }

    public ClosedObject(Value object) {
        this.object = object;
    }

    public ClosedObject update(Value newObject) {
        if(newObject == this.object)
            return this;
        return new ClosedObject(newObject);
    }

    /**
     * Closed object is already closed - no need to re-wrap
     */
    @Override
    public ClosedObject closed() {
        return this;
    }

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.closedObject(this);
    }
}
