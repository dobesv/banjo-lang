package banjo.eval;

public class ArgumentNotSupplied extends Fail {

	private String arg;

	public ArgumentNotSupplied(String arg) {
	    this.arg = arg;
    }
	
	@Override
	public String getMessage() {
		return "Missing argument: "+arg;
	}

}
