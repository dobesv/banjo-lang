package banjo.eval;


public class UnboundSlotSelfName extends Fail {
	public UnboundSlotSelfName(String message, Throwable cause) {
        super(message, cause);
    }

	public UnboundSlotSelfName(String message) {
        super(message);
    }

	public UnboundSlotSelfName(Throwable cause) {
        super(cause);
    }
}