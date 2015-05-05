package banjo.eval;


public class NotCallable extends Fail {
	final Object target;

	public NotCallable(Object target) {
	    super();
	    this.target = target;
    }

	public NotCallable(Throwable cause, Object target) {
	    super(cause);
	    this.target = target;
    }

	@Override
	public String getMessage() {
	    return "Not a function: "+target;
	}
}