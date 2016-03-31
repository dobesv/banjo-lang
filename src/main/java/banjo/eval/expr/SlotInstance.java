package banjo.eval.expr;

import java.util.function.BiFunction;

import banjo.eval.environment.Environment;
import banjo.eval.util.LazyBoundValue;
import banjo.expr.free.FreeExpression;
import banjo.expr.token.Identifier;
import banjo.value.Reactive;
import banjo.value.Value;
import fj.data.Option;

public interface SlotInstance extends BiFunction<Value, Value, Value>, Reactive<SlotInstance> {
    public static SlotInstance fromFreeExpression(Identifier slotName, Option<Identifier> optSourceObjectBinding, FreeExpression freeExpr, Environment environment) {
        return optSourceObjectBinding.map(sourceObjectBinding -> 
 recursive(slotName, sourceObjectBinding, freeExpr, environment))
            .orSome(() -> new FreeSlotInstance(new LazyBoundValue(freeExpr, environment)));
        
    }

    public static SlotInstance recursive(Identifier slotName, Identifier sourceObjectBinding, FreeExpression freeExpr, Environment environment) {
        return new RecursiveSlotInstance(slotName, sourceObjectBinding, freeExpr, environment);
    }
}
