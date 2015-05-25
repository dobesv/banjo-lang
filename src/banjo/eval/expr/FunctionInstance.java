package banjo.eval.expr;

import java.util.function.Supplier;

import banjo.eval.Value;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.expr.free.FreeExpression;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Option;

public class FunctionInstance extends Value {
	public final List<SourceFileRange> ranges;
	public final List<Identifier> args;
	public final FreeExpression body;
	public final Option<Identifier> sourceObjectBinding;
	public final Environment parentEnvironment;

	public FunctionInstance(List<SourceFileRange> ranges, List<Identifier> args,
			FreeExpression body, Option<Identifier> sourceObjectBinding, Environment parentEnvironment) {
		this.ranges = ranges;
		this.args = args;
		this.body = body;
		this.sourceObjectBinding = sourceObjectBinding;
		this.parentEnvironment = parentEnvironment;
    }


	@Override
	public Object call(Object recurse, Object prevImpl, List<Object> arguments) {
		final List<Supplier<StackTraceElement>> oldStack = JavaRuntimeSupport.stack.get();
		JavaRuntimeSupport.stack.set(oldStack.cons(this::makeStackTraceElement));
		try {
			Environment env = new FunctionEnvironment(args, arguments, sourceObjectBinding, recurse, prevImpl, parentEnvironment);
			return body.apply(env);
		} finally {
			JavaRuntimeSupport.stack.set(oldStack);
		}
	}

	public StackTraceElement makeStackTraceElement() {
		return new StackTraceElement("<banjo code>",
				sourceObjectBinding.map(x -> x.id).orSome("<function>"),
				ranges.toOption().map(x -> x.getSourceFile()).toNull(),
				ranges.toOption().map(x -> x.getStartLine()).orSome(-1));
	}
	@Override
    public String toStringFallback() {
		final Option<SourceFileRange> loc = ranges.toOption();
		Option<Identifier> bindingName = sourceObjectBinding;
		StringBuffer sb = new StringBuffer();
		sb.append("<function");
		bindingName.forEach(x -> sb.append(" ").append(x));
		loc.forEach(x -> sb.append(" from ").append(x));
		sb.append(">");
		return sb.toString();
    }
}
