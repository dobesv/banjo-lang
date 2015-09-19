package banjo.io.resource;

import fj.data.List;

public class TimerResource extends CompositeResource {

	public TimerResource() {
		super(makeResources());
	}

	public static void fps(int frequency) {
		
	}
	private static List<Resource> makeResources() {
		List<Resource> resources = List.nil();
		// Need a collection of timers ... 
		//resources = resources.cons(Resource.slot("timers", Resource.toJava(TimerResource::fps, Integer.class).slot("fps").slot("timer"));
		//resources = resources.cons(Resource.toJava(TimerResource::fps, Integer.class).slot("fps").slot("timer"));
		return resources;
	}

}
