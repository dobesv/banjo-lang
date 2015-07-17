package banjo.event.view;

import banjo.event.Event;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P;
import fj.P2;
import fj.data.List;

public class AllEventView implements EventView {
	@Override
	public P2<EventView, List<Value>> apply(Event t) {
		return P.p(this, List.single(t));
	}

	public banjo.value.Reaction<EventView> react(Event event) {
		return Reaction.none(this);
	}
}