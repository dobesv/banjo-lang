package banjo.eval.expr;

import java.util.function.Function;

import banjo.eval.environment.Environment;
import banjo.eval.util.JavaLanguageRuntimeImpl;
import banjo.event.PastEvent;
import banjo.expr.free.FreeExpression;
import banjo.expr.source.Operator;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import banjo.value.FunctionTrait;
import banjo.value.Reaction;
import banjo.value.SlotValue;
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
        final List<Value> oldStack = JavaLanguageRuntimeImpl.stackPush(this);
		try {
			Environment env = closure.enterFunction(args, arguments, sourceObjectBinding, recurse, prevImpl);
			return body.apply(env);
		} finally {
            JavaLanguageRuntimeImpl.setStack(oldStack);
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

    @Override
    public Value slot(Value self, String name, Set<SourceFileRange> ranges, Value fallback) {
        if("label".equals(name))
            return super.slot(name, ranges);
        if(Operator.FUNCTION_COMPOSITION_LEFT.getOp().equals(name))
            return Value.function(this::compose);
        return trait.slot(self, name, ranges, fallback);
    }

    @Override
    public Value callMethod(String name, Set<SourceFileRange> ranges, Value targetObject, Value fallback, List<Value> args) {
        return trait.callMethod(name, ranges, targetObject, fallback, args);
    }
}
