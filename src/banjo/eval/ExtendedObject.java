package banjo.eval;

import banjo.dom.source.Operator;
import banjo.dom.token.Key;
import fj.data.List;

public class ExtendedObject implements EvalObject {
	private final EvalObject base;
	private final EvalObject extension;
	public ExtendedObject(EvalObject base, EvalObject extension) {
		super();
		this.base = base;
		this.extension = extension;
	}
	public EvalObject getBase() {
		return base;
	}
	public EvalObject getExtension() {
		return extension;
	}

	@Override
	public EvalObject call(Key methodName, List<List<EvalObject>> argumentLists, boolean optional, EvalObject selfArg, boolean callNext) {
		// Becomes extension.?method(...) || base.method(...) for a regular non-optional call or extension.?method(...) || base.?method(...) for an optional one
		// If they use call next method, for this method name on this selfArg, use the method from base
		CallNextMethodWrapper selfWrap = new CallNextMethodWrapper(this, methodName, base, selfArg);
		EvalObject extResult = extension.call(methodName, argumentLists, true, selfWrap, callNext);
		EvalObject baseResult = new DeferredCall(base, methodName, argumentLists, optional, selfArg, callNext);
		return extResult.call(Operator.LOGICAL_OR.getMethodNameKey(), List.single(List.single(baseResult)), false, selfArg, callNext);
	}

	@Override
	public EvalObject extend(EvalObject extension) {
		return new ExtendedObject(base, extension.extend(extension));
	}



}
