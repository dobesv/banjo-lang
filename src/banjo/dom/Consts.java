package banjo.dom;

import fj.data.Option;

public interface Consts {
	static final int CODEPOINT_NONE = -1;
	static final Option<String> METHOD_NONE = Option.none();
	static final Option<ParenType> PAREN_NONE = ParenType.NONE;
	static final Option<OperatorRef> OPTOKEN_NONE = Option.none();
}