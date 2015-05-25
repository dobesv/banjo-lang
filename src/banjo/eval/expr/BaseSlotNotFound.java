package banjo.eval.expr;

import banjo.eval.SlotNotFound;
import banjo.eval.util.JavaRuntimeSupport;

public class BaseSlotNotFound extends SlotNotFound {

	public BaseSlotNotFound(String id, Object object) {
		super(id, object);
	}

	public BaseSlotNotFound(String id, Object object, Throwable cause) {
		super(id, object, cause);
	}

	@Override
	public String getMessage() {
	    // TODO Need a way to display the object without triggering another SlotNotFound error ...
		String objectStr;
        try {
	        objectStr = JavaRuntimeSupport.isDefined(object) && !id.equals("label") ? String.valueOf(object) : object == null ? "null" : object.getClass().getName()+" instance";
        } catch (Throwable t) {
        	t.printStackTrace();
        	objectStr = object.getClass().getName()+" instance";
        }
		return "No base slot '"+id+"' in "+objectStr;
	}

}
