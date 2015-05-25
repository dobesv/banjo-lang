package banjo.eval.coreexpr;

import static java.util.Objects.requireNonNull;

import java.util.function.Supplier;

import banjo.eval.UnresolvedCodeError;
import banjo.expr.core.BadCoreExpr;

public class BadExprInstance implements Supplier<Object> {

	public final BadCoreExpr badCoreExpr;

	public BadExprInstance(BadCoreExpr badCoreExpr) {
		this.badCoreExpr = requireNonNull(badCoreExpr);
	}

	@Override
	public Object get() {
		return new UnresolvedCodeError(badCoreExpr.getMessage(), badCoreExpr.getSourceFileRanges());
	}

	@Override
	public String toString() {
	    return badCoreExpr.toString();
	}
}
