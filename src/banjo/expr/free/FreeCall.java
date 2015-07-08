package banjo.expr.free;

import banjo.eval.expr.Environment;
import banjo.eval.value.Value;
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


	/**
	 * Bind the call to the environment, but do not evaluate it yet.
	 */
	@Override
	public Value apply(Environment environment) {
		Value callee = function.apply(environment);
		List<Value> args = this.args.map(arg -> arg.apply(environment));
		return Value.lazy(() -> callee.call(args));
	}

	@Override
	public String toString() {
	    return function + "(" + ListUtil.insertCommas(args) + ")";
	}
}