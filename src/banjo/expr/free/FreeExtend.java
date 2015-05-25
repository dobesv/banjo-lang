package banjo.expr.free;

import banjo.eval.ExtendedObject;
import banjo.eval.coreexpr.Environment;
import banjo.eval.util.SlotMemoizer;
import banjo.expr.source.Operator;

public class FreeExtend implements FreeExpression {
	public final FreeExpression base;
	public final FreeExpression extension;
	public FreeExtend(FreeExpression base, FreeExpression extension) {
        super();
        this.base = base;
        this.extension = extension;
    }
	@Override
	public Object apply(Environment environment) {
	    return new SlotMemoizer(new ExtendedObject(base.apply(environment), extension.apply(environment)));
	}

	@Override
	public String toString() {
	    return base + " " + Operator.EXTEND.getOp() + " " + extension;
	}
}