package banjo.dom;

import fj.data.List;
import banjo.parser.util.SourceFileRange;

public interface ExprAlgebra<T> {

	T badExpr(List<SourceFileRange> ranges, String message, Object... args);

}