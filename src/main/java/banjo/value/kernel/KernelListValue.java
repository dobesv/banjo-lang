package banjo.value.kernel;

import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.List;

public class KernelListValue extends KernelValueWrapper<List<Value>> {

    public KernelListValue(List<Value> value, Value trueValue) {
        super(value, trueValue);
    }

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.kernelList(this);
    }

}
