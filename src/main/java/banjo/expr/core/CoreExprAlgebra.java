package banjo.expr.core;

import banjo.expr.ExprAlgebra;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;


public interface CoreExprAlgebra<T> extends ExprAlgebra<T> {
	T objectLiteral(Set<SourceFileRange> ranges, List<P3<Identifier, Option<Identifier>, T>> slots);
	T numberLiteral(Set<SourceFileRange> ranges, Number value, String source, boolean kernelNumber);
	T stringLiteral(Set<SourceFileRange> ranges, String text, boolean kernelString);
	T listLiteral(Set<SourceFileRange> ranges, List<T> elements);
	T call(Set<SourceFileRange> ranges, T function, List<T> args);
	T extend(Set<SourceFileRange> ranges, T base, T extension);
	T identifier(Set<SourceFileRange> ranges, String id);
	T let(Set<SourceFileRange> ranges, List<P2<Identifier, T>> bindings, T body);

    T functionLiteral(Set<SourceFileRange> ranges, List<Identifier> args, T body, Option<Identifier> calleeBinding);
	T baseFunctionRef(Set<SourceFileRange> ranges, Identifier name);
	T projection(Set<SourceFileRange> ranges, T object, T body, boolean base);

    T kernelGlobalObject(KernelGlobalObject kernelGlobalObject);
}
