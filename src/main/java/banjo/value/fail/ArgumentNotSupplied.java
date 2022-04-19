package banjo.value.fail;

import banjo.eval.EvalContext;

public class ArgumentNotSupplied extends Fail {

	private String arg;

    public ArgumentNotSupplied(EvalContext<?> ctx, String arg) {
        super(ctx);
	    this.arg = arg;
    }
	
	@Override
	public String getMessage() {
		return "Missing argument: "+arg;
	}

}
