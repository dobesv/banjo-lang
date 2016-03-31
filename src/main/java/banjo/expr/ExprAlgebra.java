package banjo.expr;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public interface ExprAlgebra<T> {

	T badExpr(Set<SourceFileRange> ranges, String message, Object... args);

}