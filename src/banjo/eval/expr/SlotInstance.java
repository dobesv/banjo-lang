package banjo.eval.expr;

import java.util.function.BiFunction;

import banjo.value.Reactive;
import banjo.value.Value;

public interface SlotInstance extends BiFunction<Value, Value, Value>, Reactive<SlotInstance> {
}
