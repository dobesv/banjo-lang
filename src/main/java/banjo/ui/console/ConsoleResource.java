package banjo.ui.console;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.io.UnsupportedEncodingException;
import java.util.concurrent.ConcurrentLinkedQueue;

import banjo.eval.expr.ObjectLiteralInstance;
import banjo.event.FutureEvent;
import banjo.event.PastEvent;
import banjo.expr.util.SourceFileRange;
import banjo.io.resource.BaseResource;
import banjo.io.resource.CompositeResource;
import banjo.io.resource.Resource;
import banjo.value.Value;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Set;
import fj.data.TreeMap;

public class ConsoleResource extends CompositeResource {

	public ConsoleResource() {
		super(subresources());
	}

	public static void out(String text) {
		if(text == null || text.isEmpty())
			return;
		System.out.print(text);
	}

	public static void outln(String text) {
		if(text == null || text.isEmpty())
			return;
		System.out.println(text);
	}
	
	public static void err(String text) {
		if(text == null || text.isEmpty())
			return;
		System.err.print(text);
	}

	public static void errln(String text) {
		if(text == null || text.isEmpty())
			return;
		System.err.println(text);
	}
	
	static class InputLinesResource extends BaseResource implements Resource {
		Thread readerThread;
		BufferedReader lineReader;
		boolean readLines;
		ConcurrentLinkedQueue<String> lineBuffer = new ConcurrentLinkedQueue<>();
		long pollPeriod = 100;
		FutureEvent futureEvent = new FutureEvent("console input line");
		
		public void setReadLines(boolean newReadLines) {
			readLines = newReadLines;
			if(readLines == false) {
				if(readerThread != null) {
					readerThread.interrupt();
					readerThread = null; // Done with it
				}
			} else if(readLines == true) {
				if(lineReader == null) {
					try {
						lineReader = new BufferedReader(new InputStreamReader(System.in, "utf8"));
					} catch (UnsupportedEncodingException e) {
						throw new UncheckedIOException(e);
					}
				}
				if(readerThread == null) {
					readerThread = new Thread("Async Console Line Reader") {
						@Override
						public void run() {
							while(readLines) {
								try {
									String nextLine = lineReader.readLine();
									lineBuffer.add(nextLine);
								} catch (IOException e) {
									throw new UncheckedIOException(e);
								}
							}
						}
					};
					readerThread.setDaemon(true);
					readerThread.start();
				}
			}
		}
		
		static Value lineEventValue(String line, Value nextEvent) {
			ObjectLiteralInstance value = new ObjectLiteralInstance( 
					TreeMap.<String,Value>empty(Ord.stringOrd)
					.set("line", Value.fromJava(line))
					.set("next event", nextEvent)
					);
			return value;
		}
		@Override
		public P2<Resource, List<PastEvent>> poll(long timestamp) {
			if(!readLines)
				return P.p(this, List.nil());
			String nextLine;
			List.Buffer<String> newLinesBuf = new List.Buffer<String>();
			while((nextLine = lineBuffer.poll()) != null) {
				newLinesBuf.snoc(nextLine);
			}
			List<String> newLines = newLinesBuf.toList();
			Value newEvent = newLines.foldRight((line, nextEvent) -> 
				futureEvent.occurrence(timestamp, lineEventValue(line, nextEvent)), (Value)futureEvent);
			
			return P.p(this, newEvent == futureEvent ? List.nil() : List.single((PastEvent)newEvent));
		}
		
		@Override
		public long nextPollTime(long lastPollTime) {
			return readLines ? lastPollTime + pollPeriod : Long.MAX_VALUE;
		}

		@Override
		public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
			if(name.equals("java console input lines")) {
				// This should only be referenced on startup, I think ... but can I prove it ? Maybe not. 
				return futureEvent;
			}
			return Resource.super.slot(self, name, ranges, fallback);
		}
	}
	
	public static List<Resource> subresources() {
		InputLinesResource inputLines = new InputLinesResource();
		return List.list(
				inputLines,
				inputLines.futureEvent.toResource(),
				Resource.inSlot("console", Resource.inSlot("in", Resource.inSlot("read lines", 
						Resource.passChangesTo(inputLines::setReadLines, Boolean.class, Boolean.FALSE)))),
				Resource.inSlot("console", Resource.inSlot("out", Resource.passChangesTo(ConsoleResource::out, String.class))), 
				Resource.inSlot("console", Resource.inSlot("outln", Resource.passChangesTo(ConsoleResource::outln, String.class))), 
				Resource.inSlot("console", Resource.inSlot("err", Resource.passChangesTo(ConsoleResource::err, String.class))),
				Resource.inSlot("console", Resource.inSlot("errln", Resource.passChangesTo(ConsoleResource::errln, String.class)))
		);
	}	
}
