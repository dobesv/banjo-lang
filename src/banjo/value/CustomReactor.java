package banjo.value;

import banjo.event.Event;
import fj.data.List;
import javafx.beans.value.ObservableValue;

public class CustomReactor implements Value {
	public final Value reactor;
	
	public CustomReactor(Value reactor) {
		this.reactor = reactor;
	}

	@Override
	public Reaction<Value> react(Event event) {
		Value newFunction = this.reactor.call1(event);
		if(newFunction == this.reactor)
			return Reaction.of(this);
		return Reaction.of(new CustomReactor(reactor));
	}

	@Override
	public boolean isReactive() {
		return true;
	}

	public Value slot(Value self, String name, Value fallback) {
		return reactor.slot(self, name, fallback);
	}

	public Value slot(String name) {
		return reactor.slot(name);
	}

	public Value callMethod(String name, Value targetObject, Value fallback, List<Value> args) {
		return reactor.callMethod(name, targetObject, fallback, args);
	}

	public Value callMethod(String name, List<Value> args) {
		return reactor.callMethod(name, args);
	}

	@Override
	public ObservableValue<Value> toObservableValue() {
		throw new Error("Not implemented");
	}
}
