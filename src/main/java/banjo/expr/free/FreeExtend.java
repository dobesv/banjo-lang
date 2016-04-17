package banjo.expr.free;

import banjo.eval.ExtendedObject;
import banjo.eval.environment.Environment;
import banjo.expr.source.Operator;
import banjo.value.Value;
import banjo.value.meta.SlotMemoizer;
import fj.data.List;

public class FreeExtend implements FreeExpression {
	public final FreeExpression base;
	public final FreeExpression extension;
	public FreeExtend(FreeExpression base, FreeExpression extension) {
        super();
        this.base = base;
        this.extension = extension;
    }
	@Override
    public Value apply(Environment environment, List<Value> trace) {
        return new SlotMemoizer(new ExtendedObject(base.apply(environment, trace), extension.apply(environment, trace)));
	}

	@Override
	public String toString() {
	    return base + " " + Operator.EXTENSION.getOp() + " " + extension;
	}
}