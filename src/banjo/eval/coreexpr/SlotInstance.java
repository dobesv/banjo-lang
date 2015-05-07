package banjo.eval.coreexpr;

import static java.util.Objects.requireNonNull;

import java.util.function.Supplier;

import fj.P;
import fj.data.List;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.Slot;
import banjo.eval.CalculatedValue;
import banjo.eval.Value;
import banjo.eval.util.JavaRuntimeSupport;

public class SlotInstance extends Value implements CalculatedValue {
	public Slot slot;
	public Object self;
	public Object prevSlotValue;
	public CoreExprEvaluator evaluator;
	public Object cachedValue;
	public boolean calculated;

	public SlotInstance(Slot slot, CoreExprEvaluator evaluator, Object self,
            Object prevSlotValue) {
	    super();
	    this.slot = slot;
	    this.evaluator = requireNonNull(evaluator);
	    this.self = requireNonNull(self);
	    this.prevSlotValue = prevSlotValue;
    }


	public StackTraceElement makeStackTraceElement() {
		return new StackTraceElement("<banjo code>",
				slot.selfBinding.map(x -> x.id).orSome("_")+"."+slot.name,
				slot.name.getSourceFileRanges().toOption().map(x -> x.getSourceFile()).toNull(),
				slot.name.getSourceFileRanges().toOption().map(x -> x.getStartLine()).orSome(-1));
	}

	@Override
	public Object calculate() {
		if(!calculated) {
			calculated = true;
			List<Supplier<StackTraceElement>> oldStack = JavaRuntimeSupport.stack.get();
			try {
				JavaRuntimeSupport.stack.set(oldStack.cons(this::makeStackTraceElement));
				final CoreExprEvaluator slotEvaluator =
						slot.selfBinding.isSome() ?
						evaluator.child(List.iterableList(slot.selfBinding.map(id -> P.p(id, new Binding(self, slot.name.id, prevSlotValue, null))))) :
						evaluator;
				final CoreExpr slotExpr = slot.value;
				//self = null;
				cachedValue = JavaRuntimeSupport.force(slotEvaluator.evaluate(slotExpr));
			} finally {
				JavaRuntimeSupport.stack.set(oldStack);
			}
		}
	    return cachedValue;
	}

	public Object call(Object recurse, Object baseImpl, List<Object> arguments) {
		Object delegate = calculate();
		return JavaRuntimeSupport.call(delegate, recurse, baseImpl, arguments);
	}

	@Override
	public Object callMethod(String name, Object targetObject,
	        Supplier<Object> fallback, List<Object> args) {
		Object delegate = calculate();
	    return JavaRuntimeSupport.callMethod(delegate, name, targetObject, fallback, args);
	}

	@Override
	public Object slot(Object self, String name, Supplier<Object> baseSlotValue) {
		Object delegate = calculate();
	    return JavaRuntimeSupport.readSlot(delegate, self, baseSlotValue, name);
	}

	@Override
	public String toStringFallback() {
		if(self != null)
			return "lazy("+self+"."+slot.name+")";
	    return "<slot "+slot.name+" in ???>";
	}
}
