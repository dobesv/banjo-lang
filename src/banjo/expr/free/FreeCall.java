package banjo.expr.free;

import banjo.eval.Environment;
import banjo.eval.expr.CallInstance;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
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

	/**
	 * Bind the call to the environment, but do not evaluate it yet.
	 */
	@Override
	public Value apply(Environment environment) {
		Value callee = function.apply(environment);
		List<Value> args = this.args.map(arg -> arg.apply(environment));
		return new CallInstance(ranges, callee, args);
	}

	@Override
	public String toString() {
	    return function + "(" + ListUtil.insertCommas(args) + ")";
	}
}