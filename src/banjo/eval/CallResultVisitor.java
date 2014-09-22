package banjo.eval;

public interface CallResultVisitor<T> {
	/**
	 * Normal function/method return.  Nothing much to see here
	 */
	T normalReturn(Object result);

	/**
	 * A precondition of this call was not met - the caller can either handle
	 * that or fail by passing back an error() to its own caller.
	 */
	T preconditionFailed(Object description);

	/**
	 * Some other failure case - likely the precondition of an inner
	 * expression failed without being checked.
	 */
	T error(Object description);
}
