package banjo.dom.core;

import fj.data.List;
import banjo.dom.ExprAlgebra;
import banjo.dom.token.Key;
import banjo.parser.util.SourceFileRange;


public interface CoreExprAlgebra<T> extends ExprAlgebra<T> {
	T objectLiteral(List<SourceFileRange> ranges, List<T> methods);
	T stringLiteral(List<SourceFileRange> ranges, String text);
	T listLiteral(List<SourceFileRange> ranges, List<T> elements);
	T call(List<SourceFileRange> ranges, T object, List<T> nameParts, List<List<T>> argumentLists);
	T extend(List<SourceFileRange> ranges, T base, T extension);
	T inspect(List<SourceFileRange> ranges, T target);
	T method(List<SourceFileRange> sourceFileRanges, List<T> selfArg,
			List<T> nameParts, List<List<List<T>>> argumentLists,
			T body);
	
}
