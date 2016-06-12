package banjo.value.fail;

import banjo.value.Value;
import fj.data.List;

public class FailWithException extends Fail {
	public final Throwable cause;

    public FailWithException(List<Value> trace, Throwable cause) {
        super(trace);
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
        return "fail(\"" + cause.toString() + "\")";
	}

}
