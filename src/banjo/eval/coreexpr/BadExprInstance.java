package banjo.eval.coreexpr;

import java.util.function.Supplier;

import banjo.dom.core.BadCoreExpr;
import banjo.eval.UnresolvedCodeError;

public class BadExprInstance implements Supplier<Object> {

	public final BadCoreExpr badCoreExpr;

	public BadExprInstance(BadCoreExpr badCoreExpr) {
		this.badCoreExpr = badCoreExpr;
	}

	@Override
	public Object get() {
		return new UnresolvedCodeError(badCoreExpr.getMessage(), badCoreExpr.getSourceFileRanges());
	}

}
