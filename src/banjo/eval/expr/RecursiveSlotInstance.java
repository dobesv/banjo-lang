package banjo.eval.expr;

import java.awt.image.RescaleOp;

import banjo.eval.environment.Binding;
import banjo.eval.environment.Environment;
import banjo.event.PastEvent;
import banjo.expr.free.FreeExpression;
import banjo.expr.token.Identifier;
import banjo.value.Reaction;
import banjo.value.Value;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.value.ObservableValue;

public class RecursiveSlotInstance implements SlotInstance {
	public final Identifier name;
	public final Identifier sourceObjectBinding;
	public final FreeExpression valueFactory;
	public final Environment environment;
	private ObservableRecursiveSlotInstance observable;

	public RecursiveSlotInstance(Identifier name, Identifier sourceObjectBinding,
            FreeExpression valueFactory, Environment environment) {
		this.name = name;
		this.sourceObjectBinding = sourceObjectBinding;
		this.valueFactory = valueFactory;
		this.environment = environment;
    }

	public StackTraceElement makeStackTraceElement() {
		return new StackTraceElement("<banjo code>",
				sourceObjectBinding+"."+name,
				name.getSourceFileRanges().toStream().toOption().map(x -> x.getSourceFile().toString()).toNull(),
				name.getSourceFileRanges().toStream().toOption().map(x -> x.getStartLine()).orSome(-1));
	}

	@Override
	public Value apply(Value self, Value prevSlotValue) {
		final Binding binding = prevSlotValue == null ? 
				Binding.slot(self, name.id) :
				Binding.slotWithBase(self, name.id, prevSlotValue);
		Environment slotEnv =
				environment.bind(sourceObjectBinding.id, binding);
		return valueFactory.apply(slotEnv);
	}

	@Override
	public Reaction<SlotInstance> react(PastEvent event) {
		return environment.react(event).map(this::update);
	}

	@Override
	public boolean isReactive() {
		return environment.isReactive();
	}
	
	public RecursiveSlotInstance update(Environment newEnvironment) {
		if(newEnvironment == environment)
			return this;
		return new RecursiveSlotInstance(name, sourceObjectBinding, valueFactory, newEnvironment);
	}

	public static final class ObservableRecursiveSlotInstance extends ObjectBinding<SlotInstance> {
		final ObservableValue<Environment> environmentBinding;
		RecursiveSlotInstance recursiveSlotInstance;
		public ObservableRecursiveSlotInstance(RecursiveSlotInstance recursiveSlotInstance) {
			super();
			environmentBinding = recursiveSlotInstance.environment.toObservableValue();
			bind(environmentBinding);
			this.recursiveSlotInstance = recursiveSlotInstance;
		}
		
		@Override
		public void dispose() {
			unbind(environmentBinding);
		}
		
		@Override
		protected SlotInstance computeValue() {
			return recursiveSlotInstance = recursiveSlotInstance.update(environmentBinding.getValue());
		}
	}
	
	@Override
	public ObservableValue<SlotInstance> toObservableValue() {
		if(observable == null)
			observable = new ObservableRecursiveSlotInstance(this);
		return observable;
	}
	
	@Override
	public String toString() {
	    return sourceObjectBinding+"."+name;
	}
}
