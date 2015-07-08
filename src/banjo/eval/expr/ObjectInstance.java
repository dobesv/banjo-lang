package banjo.eval.expr;

import banjo.eval.value.Value;
import banjo.eval.value.ValueToStringTrait;
import banjo.expr.util.ListUtil;
import fj.data.Option;
import fj.data.TreeMap;

public class ObjectInstance extends ValueToStringTrait implements Value {
	public final TreeMap<String, SlotInstance> slots;

	public ObjectInstance(
            TreeMap<String, SlotInstance> slots) {
		this.slots = slots;
    }

	@Override
	public Value slot(Value sourceObject, String name, Value fallback) {
		final Option<SlotInstance> value = slots.get(name);
		if(value.isSome())
			return value.some().apply(sourceObject, fallback);
		return Value.super.slot(sourceObject, name, fallback);
	}

	@Override
	public String toStringFallback() {
		StringBuffer sb = new StringBuffer();
		sb.append("{");
		ListUtil.insertCommas(sb, slots, slot -> {
			sb.append(slot._1()).append(" = ...");
		});
		sb.append("}");
	    return sb.toString();
	}
}
