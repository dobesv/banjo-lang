package banjo.value.fail;

import fj.data.List;

public class ArgumentNotSupplied extends Fail {

	private String arg;

    public ArgumentNotSupplied(List<?> trace, String arg) {
        super(trace);
	    this.arg = arg;
    }
	
	@Override
	public String getMessage() {
		return "Missing argument: "+arg;
	}

}
