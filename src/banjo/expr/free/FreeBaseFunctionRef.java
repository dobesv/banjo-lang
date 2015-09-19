package banjo.expr.free;

import banjo.eval.NotCallable;
import banjo.eval.UnboundFunctionSelfName;
import banjo.eval.environment.BindingVisitor;
import banjo.eval.environment.Environment;
import banjo.value.Value;
import fj.P;
import fj.data.Option;

public class FreeBaseFunctionRef implements FreeExpression {
	public final String name;

	public FreeBaseFunctionRef(String name) {
        super();
        this.name = name;
    }

	@Override
	public Value apply(Environment env) {
		return env.get(name).acceptVisitor(new BindingVisitor<Value>() {
			
			@Override
			public Value functionRecursive(Value function) {
				return new NotCallable("No base implementation for function '"+name+"'");
			}
			
			@Override
			public Value functionRecursiveWithBase(Value function, Value baseFunction) {
				return baseFunction;
			}
			
			@Override
			public Value let(Value value) {
				return new UnboundFunctionSelfName("Not a function self-name: '"+name+"' is a regular let-bound variable");
			}
			
			@Override
			public Value slot(Value sourceObject, String slotName) {
				return new UnboundFunctionSelfName("Not a function self-name: '"+name+"' is a slot object reference");
			}
			
			@Override
			public Value slotWithBase(Value sourceObject, String slotName, Value baseSlotValue) {
				return new UnboundFunctionSelfName("Not a function self-name: '"+name+"' is a slot object reference");
			}
			
			
		});
	}
}