package banjo.eval;

import banjo.eval.util.JavaRuntimeSupport;


public class SlotNotFound extends Fail {
	public final String id;
	public final Object object;

	public SlotNotFound(String id, Object object) {
		this(id, object, null);
    }

	public SlotNotFound(String id, Object object, Throwable cause) {
		super(cause);
		this.id = id;
		this.object = object;
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
		return "No such slot '"+id+"' in "+objectStr;
	}

}