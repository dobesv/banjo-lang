package banjo.dom;

import banjo.parser.util.SourceFileRange;
import fj.data.List;

public interface ExprAlgebra<T> {

	T badExpr(List<SourceFileRange> ranges, String message, Object... args);

}