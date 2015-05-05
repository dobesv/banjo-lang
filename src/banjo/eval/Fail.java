package banjo.eval;

import fj.data.List;
import banjo.eval.util.JavaRuntimeSupport;


public class Fail extends Error {
	@Override
	public synchronized Throwable fillInStackTrace() {
		Throwable t = super.fillInStackTrace();
		final StackTraceElement[] trace = Fail.makeTrace();
		if(trace.length > 0)
			t.setStackTrace(trace);
		return t;
	}

	public static StackTraceElement[] makeTrace() {
		List<StackTraceElement> trace = JavaRuntimeSupport.stack.get().map(x -> x.get());
		return trace.array(StackTraceElement[].class);
	}

	public Fail() {
	}

	public Fail(String message, Throwable cause) {
        super(message, cause);
    }

	public Fail(String message) {
        super(message);
    }

	public Fail(Throwable cause) {
        super(cause);
    }

}