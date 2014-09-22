package banjo.eval;

import banjo.dom.token.Key;
import fj.data.List;

public class EvalUtil {

	public static boolean toBoolean(Object x) throws ContractFailure {
		if(x instanceof CoreExprObject) {
			Boolean b = ((CoreExprObject)x).toBoolean();
			if(b != null)
				return b;
		}
		if(x instanceof Boolean)
			return ((Boolean)x).booleanValue();
		throw new ContractFailure("Not a boolean: "+x);
	}

	public static EvalObject preconditionFailed(String message, boolean optional) {
		if(optional) {
			return PrimitiveListWrapper.EMPTY;
		}
		throw new ContractFailure(message);
	}

	public static EvalObject noNextMethod(Key methodName, boolean optional) {
		return preconditionFailed("No more implementations of "+methodName, optional);
	}

	public static EvalObject notEnoughArguments(boolean optional) {
		return preconditionFailed("Too few arguments", optional);
	}
}
