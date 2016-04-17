package banjo.eval.expr;

import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import banjo.value.CalculatedValue;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.List;
import fj.data.Set;

public class CallInstance extends CalculatedValue implements Value {
	public final Set<SourceFileRange> ranges;
	public final Value callee;
	public final List<Value> args;
	
	public CallInstance(Set<SourceFileRange> ranges, Value callee, List<Value> args) {
		super();
		this.ranges = ranges;
		this.callee = callee;
		this.args = args;
	}

	@Override
	public Value calculate(List<Value> trace) {
        return callee.call(trace, args);
	}
	
	public StackTraceElement makeStackTraceElement() {
		return new StackTraceElement("function",
				"apply",
				ranges.toStream().toOption().map(x -> x.getSourceFile().toString()).toNull(),
				ranges.toStream().toOption().map(x -> x.getStartLine()).orSome(-1));
	}
	
	public CallInstance update(Value callee, List<Value> args) {
		if(callee == this.callee && ListUtil.elementsEq(args, this.args))
			return this;
		return new CallInstance(ranges, callee, args);
	}
	
    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.call(this);
    }
}