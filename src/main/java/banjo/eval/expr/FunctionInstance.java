package banjo.eval.expr;

import banjo.eval.environment.Environment;
import banjo.expr.free.FreeExpression;
import banjo.expr.source.Operator;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import banjo.value.FunctionTrait;
import banjo.value.SlotValue;
import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class FunctionInstance extends FunctionTrait implements Value {
	public final Set<SourceFileRange> ranges;
	public final List<Identifier> args;
	public final FreeExpression body;
	public final Option<Identifier> sourceObjectBinding;
	public final Environment closure;
    private Value trait;

    public FunctionInstance(Set<SourceFileRange> ranges, List<Identifier> args,
			FreeExpression body, Option<Identifier> sourceObjectBinding, Environment closure) {
		this.ranges = ranges;
		this.args = args;
		this.body = body;
		this.sourceObjectBinding = sourceObjectBinding;
		this.closure = closure;
        this.trait = new SlotValue(closure.projectRootObject, Identifier.FUNCTION_TRAIT.id, SourceFileRange.EMPTY_SET);
    }

	private FunctionInstance update(Environment newEnvironment) {
		return (newEnvironment == closure)? this : new FunctionInstance(ranges, args, body, sourceObjectBinding, newEnvironment);
	}

	@Override
	public Value call(List<Value> trace, Value recurse, Value prevImpl, List<Value> arguments) {
        Environment env = closure.enterFunction(trace, args, arguments, sourceObjectBinding, recurse, prevImpl);
        return body.apply(env, trace);
	}

	public StackTraceElement makeStackTraceElement() {
		return new StackTraceElement("<banjo code>",
				sourceObjectBinding.map(x -> x.id).orSome("<function>"),
				ranges.toStream().toOption().map(x -> x.getSourceFile().toString()).toNull(),
				ranges.toStream().toOption().map(x -> x.getStartLine()).orSome(-1));
	}
	@Override
    public String toStringFallback(List<Value> trace) {
		final Option<SourceFileRange> loc = ranges.toStream().toOption();
		Option<Identifier> bindingName = sourceObjectBinding;
		StringBuffer sb = new StringBuffer();
		sb.append("<function");
		bindingName.forEach(x -> sb.append(" ").append(x));
		loc.forEach(x -> sb.append(" from ").append(x));
		sb.append(">");
		return sb.toString();
    }

    @Override
    public Value slot(List<Value> trace, Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
        if("label".equals(name))
            return super.slot(trace, self, name, ranges, fallback);
        if(Operator.FUNCTION_COMPOSITION_LEFT.getOp().equals(name))
            return Value.function(this::compose);
        return trait.slot(trace, self, name, ranges, fallback);
    }

    @Override
    public Value callMethod(List<Value> trace, String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
        return trait.callMethod(trace, name, ranges, targetObject, fallback, args);
    }

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.functionInstance(this);
    }
}
