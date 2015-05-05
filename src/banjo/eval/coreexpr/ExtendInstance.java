package banjo.eval.coreexpr;

import fj.data.Option;
import fj.data.Stream;
import banjo.dom.core.Extend;
import banjo.dom.token.Identifier;
import banjo.eval.ExtendedObject;
import banjo.parser.util.SourceFileRange;

public class ExtendInstance extends ExtendedObject {
	public final Extend extend;
	public ExtendInstance(Object base, Object extension, Extend extend) {
	    super(base, extension);
	    this.extend = extend;
    }

	@Override
    public String toStringFallback() {
	    final Option<SourceFileRange> loc = extend.getSourceFileRanges().toOption();
		StringBuffer sb = new StringBuffer();
		sb.append("<object");
		loc.forEach(x -> sb.append(" from ").append(x));
		sb.append(">");
		return sb.toString();
    }

}
