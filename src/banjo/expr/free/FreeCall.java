package banjo.expr.free;

import banjo.eval.expr.CallInstance;
import banjo.eval.expr.Environment;
import banjo.eval.util.MemoizingSupplier;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
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