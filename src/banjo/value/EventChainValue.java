package banjo.value;

import banjo.eval.Fail;
import banjo.event.PastEvent;
import fj.data.Either;
import fj.data.List;
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

	public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
		return pendingValue.call(recurse, baseImpl, arguments);
	}

	public Value call(List<Value> arguments) {
		return pendingValue.call(arguments);
	}

	public Value call1(Value v) {
		return pendingValue.call1(v);
	}

	public Value slot(Value self, String name, Value fallback) {
		return pendingValue.slot(self, name, fallback);
	}

	public Value slot(String name) {
		return pendingValue.slot(name);
	}

	public Value callMethod(String name, Value targetObject, Value fallback, List<Value> args) {
		return pendingValue.callMethod(name, targetObject, fallback, args);
	}

	public Value callMethod(String name, List<Value> args) {
		return pendingValue.callMethod(name, args);
	}

	public boolean isDefined() {
		return pendingValue.isDefined();
	}

	public <T> Either<T, Fail> convertToJava(Class<T> clazz) {
		return pendingValue.convertToJava(clazz);
	}

}
