package banjo.value.fail;

import banjo.eval.EvalContext;

public class FailWithException extends Fail {
	public final Throwable cause;

    public FailWithException(EvalContext<?> ctx, Throwable cause) {
        super(ctx);
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
