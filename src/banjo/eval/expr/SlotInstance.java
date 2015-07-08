package banjo.eval.expr;

import java.util.function.BiFunction;

import banjo.eval.value.Value;

public interface SlotInstance extends BiFunction<Value, Value, Value> {

}
