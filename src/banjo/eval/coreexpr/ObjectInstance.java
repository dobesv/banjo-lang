package banjo.eval.coreexpr;

import banjo.eval.Value;
import banjo.expr.util.ListUtil;
import fj.P;
import fj.data.TreeMap;

public class ObjectInstance extends Value {
	public final TreeMap<String, SlotInstance> slots;

	public ObjectInstance(
            TreeMap<String, SlotInstance> slots) {
		this.slots = slots;
    }

	@Override
	public Object slot(Object sourceObject, String name, Object fallback) {
		return slots
			.get(name).map(
				slot -> slot.apply(sourceObject, fallback)
			).orSome(P.lazy(() -> super.slot(sourceObject, name, fallback)));
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
