package banjo.dom.core;

import banjo.dom.ExprAlgebra;
import banjo.dom.token.Identifier;
import banjo.parser.util.SourceFileRange;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Option;


public interface CoreExprAlgebra<T> extends ExprAlgebra<T> {
	T objectLiteral(List<SourceFileRange> ranges, List<P3<Identifier, Option<Identifier>, T>> slots);
	T numberLiteral(List<SourceFileRange> ranges, Number value, String suffix);
	T stringLiteral(List<SourceFileRange> ranges, String text);
	T listLiteral(List<SourceFileRange> ranges, List<T> elements);
	T call(List<SourceFileRange> ranges, T function, List<T> args);
	T extend(List<SourceFileRange> ranges, T base, T extension);
	T inspect(List<SourceFileRange> ranges, T target);
	T identifier(List<SourceFileRange> ranges, String id);
	T let(List<SourceFileRange> ranges, List<P2<Identifier, T>> bindings, T body);
	T functionLiteral(List<SourceFileRange> ranges, List<Identifier> args, T body, Option<Identifier> recursiveBindingName);
	T slotReference(List<SourceFileRange> ranges, T object, Identifier slotName);
	T baseFunctionRef(List<SourceFileRange> sourceFileRanges, Identifier name);
}
