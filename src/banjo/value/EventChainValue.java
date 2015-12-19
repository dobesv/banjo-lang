package banjo.value;

import banjo.eval.Fail;
import banjo.event.PastEvent;
import banjo.expr.util.SourceFileRange;
import fj.data.Either;
import fj.data.List;
import fj.data.Set;
import javafx.beans.value.ObservableValue;

/**
 * Provide access to the "next event".  
 * 
 * Before an event occurs, this delegates all behavior to the pendingValue.
 *
 * When an event occurs, this calls the occurredValueFactory and morphs into
 * the result of that call.
 * 
 * The pending value and occurred value factory values are not updated in response
 * to events, they should be inert values (usually functions).
 */
public class EventChainValue implements Value {

	public final Value pendingValue;
	public final Value occurredValueFactory;
	
	public EventChainValue(Value pendingValue, Value occurredValueFactory) {
		super();
		this.pendingValue = pendingValue;
		this.occurredValueFactory = occurredValueFactory;
	}

	@Override
	public Reaction<Value> react(PastEvent event) {
		return Reaction.of(occurredValueFactory.call(List.list((Value)event, this)));
	}

	@Override
	public boolean isReactive() {
		return true;
	}

	@Override
	public ObservableValue<Value> toObservableValue() {
		return new ObservableReactive<Value>(this);
	}

    @Override
	public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
		return pendingValue.call(recurse, baseImpl, arguments);
	}

    @Override
	public Value call(List<Value> arguments) {
		return pendingValue.call(arguments);
	}

    @Override
	public Value call1(Value v) {
		return pendingValue.call1(v);
	}

    @Override
	public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
		return pendingValue.slot(self, name, ranges, fallback);
	}

    @Override
	public Value slot(String name, Set<SourceFileRange> ranges) {
		return pendingValue.slot(name, ranges);
	}

    @Override
	public Value callMethod(String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
		return pendingValue.callMethod(name, ranges, targetObject, fallback, args);
	}

    @Override
    public Value callMethod(String name, Set<SourceFileRange> ranges, List<Value> args) {
        return pendingValue.callMethod(name, ranges, args);
	}

    @Override
	public boolean isDefined() {
		return pendingValue.isDefined();
	}

	@Override
    public <T> Either<T, Fail> convertToJava(Class<T> clazz) {
		return pendingValue.convertToJava(clazz);
	}

}
