package banjo.eval;

import banjo.value.Value;
import banjo.value.kernel.KernelStringValue;
import fj.data.List;

public class FailWithMessage extends Fail {
	public final String message;
	
    public FailWithMessage(List<Value> trace, String message) {
        super(trace);
		this.message = message;
	}
	
    public FailWithMessage(List<Value> trace, Value message) {
        super(trace);
        this.message = KernelStringValue.extractString(trace, message).orSome(message::toString);
    }

	@Override
	public String getMessage() {
		return message;
	}

}
