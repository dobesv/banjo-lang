package banjo.eval.expr;

import banjo.eval.environment.Environment;
import banjo.eval.util.LazyBoundValue;
import banjo.expr.free.FreeExpression;
import banjo.expr.token.Identifier;
import banjo.value.Value;
import fj.data.List;
import fj.data.Option;

public interface SlotInstance {
    public static SlotInstance fromFreeExpression(Identifier slotName, Option<Identifier> optSourceObjectBinding, FreeExpression freeExpr, Environment environment) {
        return optSourceObjectBinding.map(sourceObjectBinding -> 
 recursive(slotName, sourceObjectBinding, freeExpr, environment))
            .orSome(() -> new FreeSlotInstance(new LazyBoundValue(freeExpr, environment)));
        
    }

    public static SlotInstance recursive(Identifier slotName, Identifier sourceObjectBinding, FreeExpression freeExpr, Environment environment) {
        return new RecursiveSlotInstance(slotName, sourceObjectBinding, freeExpr, environment);
    }

    public Value apply(List<Value> trace, Value self, Value prevSlotValue);
}
