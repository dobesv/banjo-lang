package banjo.eval;

public class FailWithMessage extends Fail {
	public final String message;
	
	public FailWithMessage(String message) {
		this.message = message;
	}
	
	@Override
	public String getMessage() {
		return message;
	}

}
