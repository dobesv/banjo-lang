package banjo.value.meta;

import banjo.value.BaseInertValue;
import banjo.value.Value;
import fj.data.List;

/**
 * Function object that, given any argument list, returns its arguments as a
 * list.
 * <p>
 * Used to construct the runtime values of list literals.
 */
public class ListFactory extends BaseInertValue implements Value {

    public final Value listWrapper;

    public ListFactory(Value listWrapper) {
        this.listWrapper = listWrapper;
    }

    @Override
    public Value call(List<Value> arguments) {
        return listWrapper.call1(Value.fromJava(arguments));
    }

    @Override
    public Value call(Value recurse, Value baseImpl, List<Value> arguments) {
        return call(arguments);
    }
}
