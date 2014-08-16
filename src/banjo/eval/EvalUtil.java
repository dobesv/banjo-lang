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


}
