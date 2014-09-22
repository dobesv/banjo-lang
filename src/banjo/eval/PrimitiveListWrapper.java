package banjo.eval;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Operator;
import banjo.dom.token.Key;
import fj.data.List;

/**
 * The primitive list - it implements only one method, function call - which takes two parameters.  The second
 * is a function to call if the list is not empty, passing the head and tail of the list, and the first is
 * a function to call if the list is empty, passing nothing.
 */
public class PrimitiveListWrapper implements EvalObject {
	public static final PrimitiveListWrapper EMPTY = new PrimitiveListWrapper(List.<EvalObject>nil());
	private final List<EvalObject> delegate;
	private @Nullable List<List<EvalObject>> callbackArgs;

	public PrimitiveListWrapper(List<EvalObject> delegate) {
		super();
		this.delegate = delegate;
	}

	@Override
	public EvalObject call(Key methodName, List<List<EvalObject>> argumentLists, boolean optional, EvalObject selfArg, boolean callNext) {
		String message = null;
		if(callNext)
			message = "No more implementations of "+methodName;
		else if(!methodName.equals(Key.ANONYMOUS))
			message = "No such method "+methodName.toSource();
		else if(argumentLists.isEmpty() || argumentLists.head().isEmpty() || argumentLists.head().tail().isEmpty())
			message = "Too few arguments";
		else {
			EvalObject target;
			List<List<EvalObject>> callbackArgs = this.callbackArgs;
			if(delegate.isEmpty()) {
				target = argumentLists.head().head();
				if(callbackArgs == null)
					callbackArgs = this.callbackArgs = List.nil();
			} else {
				target = argumentLists.head().tail().head();
				if(callbackArgs == null)
					callbackArgs = this.callbackArgs = List.single(List.<EvalObject>list(delegate.head(), new PrimitiveListWrapper(delegate.tail())));
			}
			target.call(Key.ANONYMOUS, callbackArgs, optional, selfArg, callNext);
		}

		if(optional)
			return EMPTY;
		else
			throw new ContractFailure(message);
	}

	@Override
	public EvalObject extend(EvalObject extension) {
		// TODO Auto-generated method stub
		return null;
	}

}
