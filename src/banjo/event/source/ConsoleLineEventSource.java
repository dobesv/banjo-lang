package banjo.event.source;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.concurrent.LinkedBlockingQueue;

import banjo.event.Event;
import banjo.value.Value;
import fj.P;
import fj.P2;
import fj.data.List;

public class ConsoleLineEventSource implements EventSource {
	public static final EventSource INSTANCE = new ConsoleLineEventSource();
	public static final BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
	public static final LinkedBlockingQueue<String> queue = new LinkedBlockingQueue<>();
	
	public static final Thread readerThread = new Thread("ConsoleLineReader thread") {
		public void run() {
			while(true) {
				String line;
				try {
					line = br.readLine();
				} catch (IOException e) {
					return;
				}
				if(line == null)
					return;
				queue.offer(line);
			}
		}
	};
	
	public ConsoleLineEventSource() {
	}

	@Override
	public P2<EventSource, List<Event>> poll(long timestamp) {
		if(!readerThread.isAlive()) {
			readerThread.start();
		}
		int count = queue.size();
		ArrayList<String> newStrings = new ArrayList<String>(count);
		queue.drainTo(newStrings, count);
		List<Event> newEvents = List.iterableList(newStrings).map(str -> new Event(timestamp, "console line", Value.fromJava(str)));
		return P.p(this, newEvents);
	}

	@Override
	public long nextPollTime(long lastPollTime) {
		return lastPollTime + 250;
	}

}
