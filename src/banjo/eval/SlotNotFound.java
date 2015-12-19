package banjo.eval;

import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.Set;


public class SlotNotFound extends Fail {
	public final String id;
    private final Set<SourceFileRange> ranges;
	public final Value object;
	public final Throwable cause;

    public SlotNotFound(String id, Set<SourceFileRange> ranges, Value object) {
        this(id, ranges, object, null);
    }

    public SlotNotFound(String id, Set<SourceFileRange> ranges, Value object, Throwable cause) {
		this.id = id;
        this.ranges = ranges;
		this.object = object;
		this.cause = cause;
    }

	@Override
	public String getMessage() {
	    // TODO Need a way to display the object without triggering another SlotNotFound error ...
		Object actualObject = object.force();
		String objectStr;
		if(actualObject == null) {
			objectStr = "null";
		} else {
	        try {
		        objectStr = !id.equals("label") ? String.valueOf(object) : actualObject == null ? "null" : object.getClass().getName()+" instance";
	        } catch (Throwable t) {
	        	t.printStackTrace();
	        	objectStr = actualObject.getClass().getName()+" instance";
	        }
		}
        String msg = "No slot named '" + id + "' in " + objectStr;
        if(!getRanges().isEmpty()) {
            msg = getRanges().iterator().next() + ": " + msg;
        }
        return msg;
	}

	@Override
	public Throwable getCause() {
		return cause;
	}

    @Override
    public Set<SourceFileRange> getRanges() {
        return ranges;
    }
}