package banjo.io.resource;

import com.sun.javafx.binding.ObjectConstant;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P;
import fj.P2;
import fj.data.List;
import javafx.beans.value.ObservableValue;

public abstract class BaseResource implements Resource {
	@Override
	public ObservableValue<Value> toObservableValue() {
		return ObjectConstant.valueOf(this);
	}

	@Override
	public Reaction<Value> react(Event event) {
		return Reaction.of(this);
	}

	@Override
	public boolean isReactive() {
		return false;
	}

	@Override
	public long nextPollTime(long lastPollTime) {
		return Long.MAX_VALUE;
	}

	@Override
	public P2<Resource, List<Event>> poll(long timestamp) {
		return P.p(this, List.nil());
	}

}
