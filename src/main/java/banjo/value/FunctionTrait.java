package banjo.value;

import banjo.eval.EvalContext;
import banjo.expr.util.SourceFileRange;
import banjo.value.special.FunctionComposition;
import fj.data.Option;
import fj.data.Set;

/**
 * Inherit all the slots defined for a function for this value.
 */
public abstract class FunctionTrait implements Value {

    public final Value trait;

    public FunctionTrait(Value trait) {
        super();
        this.trait = trait;
    }

    @Override
    public Value slot(EvalContext<Value> ctx, Value self, String name, Set<SourceFileRange> ranges, Option<Value> fallback) {
        if (!name.equals("label")) {
            return trait.slot(ctx, self, name, ranges, fallback);
		} else {
			return Value.super.slot(ctx, self, name, ranges, fallback);
		}
	}

	public Value compose(Value functionAfter) {
        return new FunctionComposition(functionAfter, this, this.trait);
	}

    public Value getTrait() {
        return trait;
    }

}
