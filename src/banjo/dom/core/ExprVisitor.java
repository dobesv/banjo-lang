package banjo.dom.core;

import fj.data.List;
import banjo.parser.util.SourceFileRange;

public interface ExprVisitor<T> {
	T badExpr(List<SourceFileRange> ranges, String messageTemplate, Object... args);

}