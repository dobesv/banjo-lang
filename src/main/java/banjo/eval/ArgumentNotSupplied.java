package banjo.eval;

import banjo.value.Value;
import fj.data.List;

public class ArgumentNotSupplied extends Fail {

	private String arg;

    public ArgumentNotSupplied(List<Value> trace, String arg) {
        super(trace);
	    this.arg = arg;
    }
	
	@Override
	public String getMessage() {
		return "Missing argument: "+arg;
	}

}
