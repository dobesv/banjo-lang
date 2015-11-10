package banjo.eval;

public class FailWithException extends Fail {
	public final Throwable cause;
	public FailWithException(Throwable cause) {
		this.cause = cause;
	}
	
	@Override
	public Throwable getCause() {
		return cause;
	}
	
	@Override
	public String getMessage() {
		return cause.getMessage();
	}
	
	@Override
	public String toString() {
		return cause.toString();
	}

}
