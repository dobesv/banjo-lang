package banjo.event.sink;

import banjo.event.Event;

public class ConsoleEventSink implements EventSink {

	public static final String LOG_EVENT_VARIANT = "log";

	public ConsoleEventSink() {
	}

	@Override
	public void accept(Event t) {
		if(t.variant.equals(LOG_EVENT_VARIANT)) {
			t.args.forEach(x -> System.out.print(x.toString()+" "));
			System.out.println();
		}
	}

}
