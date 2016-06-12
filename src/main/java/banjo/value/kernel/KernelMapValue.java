package banjo.value.kernel;

import banjo.value.Value;
import banjo.value.ValueVisitor;
import fj.data.TreeMap;

public class KernelMapValue extends KernelValueWrapper<TreeMap<String, Value>> {

    public KernelMapValue(TreeMap<String, Value> value, Value trueValue) {
        super(value, trueValue);
    }

    @Override
    public <T> T acceptVisitor(ValueVisitor<T> visitor) {
        return visitor.kernelMap(this);
    }

}
