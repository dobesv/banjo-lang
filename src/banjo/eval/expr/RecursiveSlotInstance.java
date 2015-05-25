package banjo.eval.expr;

import banjo.expr.free.FreeExpression;
import banjo.expr.token.Identifier;

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
	public Object apply(Object self, Object prevSlotValue) {
		final BindingInstance binding = new BindingInstance(self, name.id, prevSlotValue, null);
		Environment slotEnv = ((key) ->
			key.equals(sourceObjectBinding.id) ? binding : environment.apply(key));
		return valueFactory.apply(slotEnv);
	}

	@Override
	public String toString() {
	    return sourceObjectBinding+"."+name;
	}
}
