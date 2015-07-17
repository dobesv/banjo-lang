package banjo.eval.expr;

import banjo.eval.environment.Environment;
import banjo.event.Event;
import banjo.expr.free.FreeExpression;
import banjo.expr.token.Identifier;
import banjo.value.Reaction;
import banjo.value.Value;

public class RecursiveSlotInstance implements SlotInstance {
	public final Identifier name;
	public final Identifier sourceObjectBinding;
	public final FreeExpression valueFactory;
	public final Environment environment;

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
				name.getSourceFileRanges().toOption().map(x -> x.getSourceFile()).toNull(),
				name.getSourceFileRanges().toOption().map(x -> x.getStartLine()).orSome(-1));
	}

	@Override
	public Value apply(Value self, Value prevSlotValue) {
		final BindingInstance binding = BindingInstance.slotSourceObject(self, name.id, prevSlotValue);
		Environment slotEnv =
				new SingleBindingEnvironment(sourceObjectBinding.id, binding, environment);
		return valueFactory.apply(slotEnv);
	}

	@Override
	public Reaction<SlotInstance> react(Event event) {
		return environment.react(event).map(this::update);
	}

	public SlotInstance update(Environment newEnvironment) {
		if(newEnvironment == environment)
			return this;
		return new RecursiveSlotInstance(name, sourceObjectBinding, valueFactory, newEnvironment);
	}

	@Override
	public String toString() {
	    return sourceObjectBinding+"."+name;
	}
}
