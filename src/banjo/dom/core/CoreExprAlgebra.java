package banjo.dom.core;

import banjo.dom.ExprAlgebra;
import banjo.parser.util.SourceFileRange;
import fj.data.List;


public interface CoreExprAlgebra<T> extends ExprAlgebra<T> {
	T objectLiteral(List<SourceFileRange> ranges, List<T> methods);
	T numberLiteral(List<SourceFileRange> ranges, Number value, String suffix);
	T stringLiteral(List<SourceFileRange> ranges, String text);
	T listLiteral(List<SourceFileRange> ranges, List<T> elements);
	T call(List<SourceFileRange> ranges, T object, T name, List<List<T>> argumentLists, boolean optional, boolean callNext);
	T extend(List<SourceFileRange> ranges, T base, T extension);
	T inspect(List<SourceFileRange> ranges, T target);
	T method(List<SourceFileRange> ranges, T selfArg, T name, List<List<T>> argumentLists, T precondition, T body, T postcondition);
	T identifier(List<SourceFileRange> ranges, String id);
	T mixfixFunctionIdentifier(List<SourceFileRange> sourceFileRanges, List<String> parts);
	T anonymous();
}
