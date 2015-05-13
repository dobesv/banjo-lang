package banjo.eval.coreexpr;

import java.util.function.Supplier;

import banjo.dom.core.Slot;
import banjo.dom.token.Identifier;
import banjo.eval.Fail;
import banjo.eval.SlotNotFound;
import banjo.eval.Value;
import banjo.parser.util.ListUtil;
import fj.P;
import fj.data.List;
import fj.data.Option;
import fj.data.TreeMap;

public class ObjectInstance extends Value {

	// A slot is a function from (__self, __prev_value) -> value
	public final TreeMap<String,Slot> slots;
	public final CoreExprEvaluator evaluator;

	public ObjectInstance(TreeMap<String, Slot> slots,
            CoreExprEvaluator evaluator) {
	    super();
	    this.slots = slots;
	    this.evaluator = evaluator;
    }

	@Override
	public Object slot(Object self, String name, Supplier<Object> fallback) {
		return slots
			.get(name).map(
				slot -> (Object)new SlotInstance(slot, evaluator, self, fallback)
			).orSome(P.lazy(() -> super.slot(self, name, fallback)));
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
