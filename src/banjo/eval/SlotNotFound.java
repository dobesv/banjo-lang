package banjo.eval;

import banjo.value.Value;


public class SlotNotFound extends Fail {
	public final String id;
	public final Value object;

	public SlotNotFound(String id, Value object) {
		this(id, object, null);
    }

	public SlotNotFound(String id, Value object, Throwable cause) {
		super(cause);
		this.id = id;
		this.object = object;
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
		return "No slot named '"+id+"' in "+objectStr;
	}

}