package banjo.value;

import banjo.event.PastEvent;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;
import javafx.beans.value.ObservableValue;

public class CustomReactor implements Value {
	public final Value reactor;
	
	public CustomReactor(Value reactor) {
		this.reactor = reactor;
	}

	@Override
	public Reaction<Value> react(PastEvent event) {
		Value newFunction = this.reactor.call1(event);
		if(newFunction == this.reactor)
			return Reaction.of(this);
		return Reaction.of(new CustomReactor(reactor));
	}

	@Override
	public boolean isReactive() {
		return true;
	}

    @Override
	public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
		return reactor.slot(self, name, ranges, fallback);
	}

    @Override
	public Value slot(String name, Set<SourceFileRange> ranges) {
		return reactor.slot(name, ranges);
	}

    @Override
	public Value callMethod(String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
		return reactor.callMethod(name, ranges, targetObject, fallback, args);
	}

    @Override
    public Value callMethod(String name, Set<SourceFileRange> ranges, List<Value> args) {
        return reactor.callMethod(name, ranges, args);
	}

	@Override
	public ObservableValue<Value> toObservableValue() {
		throw new Error("Not implemented");
	}
}
