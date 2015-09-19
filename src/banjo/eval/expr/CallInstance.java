package banjo.eval.expr;

import java.util.function.Supplier;

import banjo.eval.util.JavaRuntimeSupport;
import banjo.event.Event;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import banjo.value.CalculatedValue;
import banjo.value.Reaction;
import banjo.value.Value;
import fj.P2;
import fj.data.List;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

public class CallInstance extends CalculatedValue implements Value {
	public final List<SourceFileRange> ranges;
	public final Value callee;
	public final List<Value> args;
	private ObservableCallInstance observable;
	
	public CallInstance(List<SourceFileRange> ranges, Value callee, List<Value> args) {
		super();
		this.ranges = ranges;
		this.callee = callee;
		this.args = args;
	}

	@Override
	public Value calculate() {
		List<Supplier<StackTraceElement>> oldStack = JavaRuntimeSupport.stack.get();
		try {
			JavaRuntimeSupport.stack.set(oldStack.cons(this::makeStackTraceElement));
			return callee.call(args);
		} finally {
			JavaRuntimeSupport.stack.set(oldStack);
		}
	}
	
	public StackTraceElement makeStackTraceElement() {
		return new StackTraceElement("function",
				"apply",
				ranges.toOption().map(x -> x.getSourceFile().toString()).toNull(),
				ranges.toOption().map(x -> x.getStartLine()).orSome(-1));
	}
	
	@Override
	public String toString() {
		return get().toString();
	}
	
	@Override
	public boolean isCalculationReactive() {
		return callee.isReactive() || args.exists(v -> v.isReactive());
	}
	
	@Override
	public Reaction<Value> calculationReact(Event event) {
		return Reaction.p(Reaction.to(callee, event), Reaction.to(args, event)).map(P2.tuple(this::update));
	}
	
	public CallInstance update(Value callee, List<Value> args) {
		if(callee == this.callee && ListUtil.elementsEq(args, this.args))
			return this;
		return new CallInstance(ranges, callee, args);
	}
	
	public static final class ObservableCallInstance extends ObjectBinding<Value> {
		public final ObservableValue<Value> calleeBinding;
		public final List<ObservableValue<Value>> argsBinding;
		CallInstance callInstance;
		public ObservableCallInstance(CallInstance callInstance) {
			super();
			calleeBinding = callInstance.callee.toObservableValue();
			argsBinding = callInstance.args.map(Value::toObservableValue);
			this.callInstance = callInstance;
			bind(calleeBinding);
			argsBinding.forEach(this::bind);
		}
		
		@Override
		protected Value computeValue() {
			return callInstance = callInstance.update(calleeBinding.getValue(), argsBinding.map(ObservableValue::getValue));
		}
		
	}
	
	@Override
	public ObservableValue<Value> toObservableValue() {
		if(this.observable == null)
			this.observable = new ObservableCallInstance(this);
		return this.observable;
	}
}