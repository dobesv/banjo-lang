package banjo.eval.coreexpr;

import banjo.dom.source.Operator;
import banjo.eval.ExtendedObject;

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
	    return new ExtendedObject(base.apply(environment), extension.apply(environment));
	}

	@Override
	public String toString() {
	    return base + " " + Operator.EXTEND.getOp() + " " + extension;
	}
}