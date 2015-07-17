package banjo.expr.free;

import java.util.function.Supplier;

import banjo.eval.environment.Environment;
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

	public static class LazyCall implements Supplier<Value> {
		public final Value callee;
		public final List<Value> args;
		
		public LazyCall(Value callee, List<Value> args) {
			super();
			this.callee = callee;
			this.args = args;
		}

		@Override
		public Value get() {
			return callee.call(args);
		}
		
		@Override
		public String toString() {
			return get().toString();
		}
	}

	/**
	 * Bind the call to the environment, but do not evaluate it yet.
	 */
	@Override
	public Value apply(Environment environment) {
		Value callee = function.apply(environment);
		List<Value> args = this.args.map(arg -> arg.apply(environment));
		return Value.lazy(new LazyCall(callee, args));
	}

	@Override
	public String toString() {
	    return function + "(" + ListUtil.insertCommas(args) + ")";
	}
}