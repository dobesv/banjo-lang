package banjo.eval.expr;

import java.util.function.Function;
import java.util.function.Supplier;

import banjo.eval.environment.Environment;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.event.PastEvent;
import banjo.expr.free.FreeExpression;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import banjo.value.FunctionTrait;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

public class FunctionInstance extends FunctionTrait implements Value, Function<List<Value>, Value> {
	public final Set<SourceFileRange> ranges;
	public final List<Identifier> args;
	public final FreeExpression body;
	public final Option<Identifier> sourceObjectBinding;
	public final Environment closure;
	private ObservableFunctionInstance observable;

	public FunctionInstance(Set<SourceFileRange> ranges, List<Identifier> args,
			FreeExpression body, Option<Identifier> sourceObjectBinding, Environment closure) {
		this.ranges = ranges;
		this.args = args;
		this.body = body;
		this.sourceObjectBinding = sourceObjectBinding;
		this.closure = closure;
    }

	@Override
	public Reaction<Value> react(PastEvent event) {
		return closure.react(event).map(this::update);
	}
	
	@Override
	public boolean isReactive() {
		return closure.isReactive();
	}

	private FunctionInstance update(Environment newEnvironment) {
		return (newEnvironment == closure)? this : new FunctionInstance(ranges, args, body, sourceObjectBinding, newEnvironment);
	}

	public static final class ObservableFunctionInstance extends ObjectBinding<Value> {
		public final ObservableValue<Environment> closureBinding;
		public FunctionInstance functionInstance;
		public ObservableFunctionInstance(FunctionInstance functionInstance) {
			super();
			closureBinding = functionInstance.closure.toObservableValue();
			bind(closureBinding);
			this.functionInstance = functionInstance;
		}
		
		@Override
		protected Value computeValue() {
			return functionInstance = functionInstance.update(closureBinding.getValue());
		}
		
		@Override
		public void dispose() {
			unbind(closureBinding);
		}
		
	}
	
	@Override
	public ObservableValue<Value> toObservableValue() {
		if(observable == null)
			observable = new ObservableFunctionInstance(this);
		return observable;
	}
	
	@Override
	public Value call(Value recurse, Value prevImpl, List<Value> arguments) {
		final List<Supplier<StackTraceElement>> oldStack = JavaRuntimeSupport.stack.get();
		JavaRuntimeSupport.stack.set(oldStack.cons(this::makeStackTraceElement));
		try {
			Environment env = closure.enterFunction(args, arguments, sourceObjectBinding, recurse, prevImpl);
			return body.apply(env);
		} finally {
			JavaRuntimeSupport.stack.set(oldStack);
		}
	}

	public StackTraceElement makeStackTraceElement() {
		return new StackTraceElement("<banjo code>",
				sourceObjectBinding.map(x -> x.id).orSome("<function>"),
				ranges.toStream().toOption().map(x -> x.getSourceFile().toString()).toNull(),
				ranges.toStream().toOption().map(x -> x.getStartLine()).orSome(-1));
	}
	@Override
    public String toStringFallback() {
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
	public Value apply(List<Value> args) {
	    return call(args);
	}

}
