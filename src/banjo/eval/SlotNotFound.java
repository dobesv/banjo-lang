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
		Object actualObject = JavaRuntimeSupport.force(object);
		String objectStr;
		if(actualObject == null) {
			objectStr = "null";
		} else {
	        try {
		        objectStr = JavaRuntimeSupport.isDefined(actualObject) && !id.equals("label") ? String.valueOf(object) : actualObject == null ? "null" : object.getClass().getName()+" instance";
	        } catch (Throwable t) {
	        	t.printStackTrace();
	        	objectStr = actualObject.getClass().getName()+" instance";
	        }
		}
		return "No such slot '"+id+"' in "+objectStr;
	}

}