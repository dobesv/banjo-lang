package banjo.eval;

import banjo.dom.token.Key;
import fj.data.List;

/**
 * When calling a "next" method with particular name, this bumps the method "offset" by one.
 *
 */
public class CallNextMethodWrapper implements EvalObject {
	private final EvalObject target;
	private final Key methodName;
	private final EvalObject nextTarget;
	private final EvalObject nextSelfArg;


	public CallNextMethodWrapper(EvalObject target, Key methodName, EvalObject nextTarget, EvalObject nextSelfArg) {
		super();
		this.target = target;
		this.methodName = methodName;
		this.nextTarget = nextTarget;
		this.nextSelfArg = nextSelfArg;
	}

	@Override
	public EvalObject call(Key methodName, List<List<EvalObject>> argumentLists, boolean optional, EvalObject selfArg, boolean callNext) {
		if(callNext && methodName.equals(this.methodName))
			return nextTarget.call(methodName, argumentLists, optional, nextSelfArg, false);
		else
			return target.call(methodName, argumentLists, optional, selfArg, callNext);
	}

	@Override
	public EvalObject extend(EvalObject extension) {
		return target.extend(extension);
	}


}
