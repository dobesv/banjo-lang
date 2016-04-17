package banjo.expr.free;

import banjo.eval.environment.Environment;
import banjo.eval.expr.CallInstance;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.List;
import fj.data.Set;

public class FreeCall implements FreeExpression {
	public final Set<SourceFileRange> ranges;
	public final FreeExpression function;
	public final List<FreeExpression> args;
	public FreeCall(Set<SourceFileRange> ranges, FreeExpression function,
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
    public Value apply(Environment environment, List<Value> trace) {
        Value callee = function.apply(environment, trace);
        List<Value> args = this.args.map(arg -> arg.apply(environment, trace));
		return new CallInstance(ranges, callee, args);
	}

	@Override
	public String toString() {
	    return function + "(" + ListUtil.insertCommas(args) + ")";
	}
}