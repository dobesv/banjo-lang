package banjo.eval;

import banjo.dom.token.Key;
import fj.data.List;

public interface EvalObject {
	/**
	 * Call the given method on this object with the given arguments.
	 *
	 * @param methodName
	 * @param argumentLists
	 * @param optional
	 * @param selfArg An EvalObject that calls back to "self" should be directed to for this method invokation; it should
	 *    handle the "callNext" option on a call by skipping past the implementation that was actually called.  Typically this will
	 *    be a wrapper which checks that the method matches the one call
	 * @param callNext If true, activate the "call next implementation" logic.  Note that "normal" objects should always fail if they
	 *    get this - only the special wrappers that track what the next method should be based on the current one will support this.
	 *    Any other call would be an error; the precondition is that the caller is actually inside the given method at the time.
	 * @return
	 */
	public EvalObject call(Key methodName, List<List<EvalObject>> argumentLists, boolean optional, EvalObject selfArg, boolean callNext);

	public EvalObject extend(EvalObject extension);
}
