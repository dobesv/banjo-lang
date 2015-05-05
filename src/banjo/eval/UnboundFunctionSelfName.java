package banjo.eval;


public class UnboundFunctionSelfName extends Fail {
	public UnboundFunctionSelfName(String message, Throwable cause) {
        super(message, cause);
    }

	public UnboundFunctionSelfName(String message) {
        super(message);
    }

	public UnboundFunctionSelfName(Throwable cause) {
        super(cause);
    }
}