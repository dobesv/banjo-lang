package banjo.eval;

import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.Either;
import fj.data.List;
import fj.data.Set;

/**
 * Wrap an object so that if it is extended or used to extend another object, it
 * won't use slots from it's new base or extension in its calculation.
 */
public class ClosedObject implements Value {
    final Value object;

    @Override
    public Value call(List<Value> trace, Value recurse, Value baseImpl, List<Value> arguments) {
        return object.call(trace, arguments);
    }

    @Override
    public Value call(List<Value> trace, List<Value> arguments) {
        return object.call(trace, arguments);
    }

    @Override
    public Value call1(List<Value> trace, Value v) {
        return object.call1(trace, v);
    }

    @Override
    public Value slot(List<Value> trace, Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
        return slot(trace, name, ranges);
    }

    @Override
    public Value slot(List<Value> trace, String name, Set<SourceFileRange> ranges) {
        return object.slot(trace, name, ranges);
    }

    @Override
    public Value callMethod(List<Value> trace, String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
        return object.callMethod(trace, name, ranges, args);
    }

    @Override
    public Value callMethod(List<Value> trace, String name, Set<SourceFileRange> ranges, List<Value> args) {
        return object.callMethod(trace, name, ranges, args);
    }

    @Override
    public Value force(List<Value> trace) {
        return object.force(trace);
    }

    @Override
    public boolean isDefined(List<Value> trace) {
        return object.isDefined(trace);
    }

    @Override
    public <T> Either<T, Fail> convertToJava(List<Value> trace, Class<T> clazz) {
        return object.convertToJava(trace, clazz);
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
