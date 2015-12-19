package banjo.eval.expr;

import banjo.eval.SlotNotFound;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.Set;

public class BaseSlotNotFound extends SlotNotFound {

    public BaseSlotNotFound(String id, Set<SourceFileRange> ranges, Value object) {
        super(id, ranges, object);
	}

    public BaseSlotNotFound(String id, Set<SourceFileRange> ranges, Value object, Throwable cause) {
        super(id, ranges, object, cause);
	}

	@Override
	public String getMessage() {
	    // TODO Need a way to display the object without triggering another SlotNotFound error ...
		String objectStr;
        try {
	        objectStr = object.isDefined() && !id.equals("label") ? String.valueOf(object) : object == null ? "null" : object.getClass().getName()+" instance";
        } catch (Throwable t) {
        	t.printStackTrace();
        	objectStr = object.getClass().getName()+" instance";
        }
		return "No base slot '"+id+"' in "+objectStr;
	}

}
