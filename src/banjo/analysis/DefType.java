package banjo.analysis;

public enum DefType {
	/** A local variable that is set to a function literal */
	LOCAL_FUNCTION,
	/** A local variable that is initialized to a literal value */
	LOCAL_CONST,
	/** A local variable that is not a function or constant */
	LOCAL_VALUE,
	/** A function parameter */
	PARAMETER,
	/** A reference to the specific object a method was invoked upon */
	SELF,
	/** A field in an object is labeled as a "field" when referred to using dot notation */
	FIELD,
	/** A field referred to directly by name (not using a dot) */
	SELF_FIELD,
	/** A field referred to directly by name (not using a dot) and is a function literal */
	SELF_CONST,
	/** A field referred to directly by name (not using a dot) and is a function literal */
	SELF_METHOD,
	/** A reference with no matching definition */
	FREE;
}
