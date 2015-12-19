package banjo.eval;

import banjo.event.PastEvent;
import banjo.expr.util.SourceFileRange;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.data.Either;
import fj.data.List;
import fj.data.Set;
import javafx.beans.value.ObservableValue;

/**
 * Wrap an object so that if it is extended or used to extend another object, it
 * won't use slots from it's new base or extension in its calculation.
 */
public class ClosedObject implements Value {
    final Value object;

    @Override
    public boolean isReactive() {
        return object.isReactive();
    }

    @Override
    public ObservableValue<Value> toObservableValue() {
        return object.toObservableValue();
    }

    @Override
    public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
        return object.call(arguments);
    }

    @Override
    public Value call(List<Value> arguments) {
        return object.call(arguments);
    }

    @Override
    public Value call1(Value v) {
        return object.call1(v);
    }

    @Override
    public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
        return slot(name, ranges);
    }

    @Override
    public Value slot(String name, Set<SourceFileRange> ranges) {
        return object.slot(name, ranges);
    }

    @Override
    public Value callMethod(String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
        return object.callMethod(name, ranges, args);
    }

    @Override
    public Value callMethod(String name, Set<SourceFileRange> ranges, List<Value> args) {
        return object.callMethod(name, ranges, args);
    }

    @Override
    public Value force() {
        return object.force();
    }

    @Override
    public boolean isDefined() {
        return object.isDefined();
    }

    @Override
    public <T> Either<T, Fail> convertToJava(Class<T> clazz) {
        return object.convertToJava(clazz);
    }

    public ClosedObject(Value object) {
        this.object = object;
    }

    @Override
    public Reaction<Value> react(PastEvent event) {
        return object.react(event).map(this::update);
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

}
