package banjo.eval;


public class UnboundIdentifier extends Fail {
	public UnboundIdentifier(String message, Throwable cause) {
        super(message, cause);
    }

	public UnboundIdentifier(String message) {
        super(message);
    }

	public UnboundIdentifier(Throwable cause) {
        super(cause);
    }
}