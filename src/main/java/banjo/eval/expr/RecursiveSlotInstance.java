package banjo.eval.expr;

import banjo.eval.environment.Binding;
import banjo.eval.environment.Environment;
import banjo.expr.free.FreeExpression;
import banjo.expr.token.Identifier;
import banjo.value.Value;
import fj.data.List;

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
				name.getSourceFileRanges().toStream().toOption().map(x -> x.getSourceFile().toString()).toNull(),
				name.getSourceFileRanges().toStream().toOption().map(x -> x.getStartLine()).orSome(-1));
	}

	@Override
    public Value apply(List<Value> trace, Value self, Value prevSlotValue) {
		final Binding binding = prevSlotValue == null ? 
				Binding.slot(self, name.id) :
				Binding.slotWithBase(self, name.id, prevSlotValue);
		Environment slotEnv =
				environment.bind(sourceObjectBinding.id, binding);
        return valueFactory.apply(slotEnv, trace);
	}

	public RecursiveSlotInstance update(Environment newEnvironment) {
		if(newEnvironment == environment)
			return this;
		return new RecursiveSlotInstance(name, sourceObjectBinding, valueFactory, newEnvironment);
	}

	@Override
	public String toString() {
	    return sourceObjectBinding+"."+name;
	}
}
