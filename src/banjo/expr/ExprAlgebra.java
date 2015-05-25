package banjo.expr;

import banjo.expr.util.SourceFileRange;
import fj.data.List;

public interface ExprAlgebra<T> {

	T badExpr(List<SourceFileRange> ranges, String message, Object... args);

}