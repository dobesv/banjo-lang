package banjo.eval.coreexpr;

import banjo.eval.util.MemoizingSupplier;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class FreeCall implements FreeExpression {
	public final List<SourceFileRange> ranges;
	public final FreeExpression function;
	public final List<FreeExpression> args;
	public FreeCall(List<SourceFileRange> ranges, FreeExpression function,
            List<FreeExpression> args) {
        super();
        this.ranges = ranges;
        this.function = function;
        this.args = args;
    }

	@Override
	public Object apply(Environment environment) {
	    return new MemoizingSupplier<Object>(new CallInstance(function.apply(environment), args.map(arg -> arg.apply(environment))));
	}

	@Override
	public String toString() {
	    return function + "(" + ListUtil.insertCommas(args) + ")";
	}
}