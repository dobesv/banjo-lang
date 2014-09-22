package banjo.eval;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.CoreExpr;
import banjo.dom.token.Key;
import fj.data.List;
import fj.data.TreeMap;

public class DeferredCall implements EvalObject {
	private final EvalObject target;
	private final Key methodName;
	private final List<List<EvalObject>> argumentLists;
	private final boolean optional;
	private final EvalObject selfArg;
	private final boolean callNext;
	private @Nullable EvalObject result;

	public DeferredCall(EvalObject target, Key methodName, List<List<EvalObject>> argumentLists, boolean optional, EvalObject selfArg, boolean callNext) {
		this.target = target;
		this.methodName = methodName;
		this.argumentLists = argumentLists;
		this.optional = optional;
		this.selfArg = selfArg;
		this.callNext = callNext;
	}

	public @Nullable EvalObject getResult() {
		return result;
	}

	public void setResult(EvalObject result) {
		this.result = result;
	}

	public EvalObject getTarget() {
		return target;
	}

	public Key getMethodName() {
		return methodName;
	}

	public List<List<EvalObject>> getArgumentLists() {
		return argumentLists;
	}

	@Override
	public EvalObject call(Key methodName, List<List<EvalObject>> argumentLists, boolean optional, EvalObject selfArg, boolean callNext) {
		return force().call(methodName, argumentLists, optional, selfArg, callNext);
	}

	public EvalObject force() {
		EvalObject result = this.result;
		if(result == null) {
			this.result = result = target.call(methodName, argumentLists, optional, selfArg, callNext);
		}
		return result;
	}

	@Override
	public EvalObject extend(EvalObject extension) {
		return new ExtendedObject(this, extension);
	}
}
