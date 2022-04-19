package banjo.value.fail;

import banjo.eval.EvalContext;
import banjo.value.Value;

public class FailWithMessage extends Fail {
	public final String message;
	
    public FailWithMessage(EvalContext<?> ctx, String message) {
        super(ctx);
		this.message = message;
	}
	
    public FailWithMessage(EvalContext<?> ctx, Value message) {
        this(ctx, message.toString());
    }

	@Override
	public String getMessage() {
		return message;
	}

}
