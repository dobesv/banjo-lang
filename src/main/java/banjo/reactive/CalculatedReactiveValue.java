package banjo.reactive;

import banjo.value.Value;
import fj.data.List;

public abstract class CalculatedReactiveValue {

    Value memo;

    public Value currentValue(List<ReactiveValue> trace) {
        if(memo == null)
            memo = this.calculateCurrentValue(trace);
        return memo;
    }

    public abstract Value calculateCurrentValue(List<ReactiveValue> trace);
}
