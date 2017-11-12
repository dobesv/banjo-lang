package banjo.expr.core;

import banjo.expr.ExprAlgebra;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Set;


public interface CoreExprAlgebra<T> extends ExprAlgebra<T> {
    T objectLiteral(Set<SourceFileRange> ranges, List<P3<Identifier, List<Identifier>, T>> slots);

    T numberLiteral(Set<SourceFileRange> ranges, Number value, String source);

    T kernelNumberLiteral(Set<SourceFileRange> ranges, Number value, String source);

    T stringLiteral(Set<SourceFileRange> ranges, String text);

    T kernelStringLiteral(Set<SourceFileRange> ranges, String text);

	T listLiteral(Set<SourceFileRange> ranges, List<T> elements);

	T extend(Set<SourceFileRange> ranges, T base, T extension);

	T identifier(Set<SourceFileRange> ranges, String id);

	T let(Set<SourceFileRange> ranges, List<P2<Identifier, T>> bindings, T body);

    T projection(Set<SourceFileRange> ranges, List<T> slotArgs, T body);

    T kernelGlobalObject(KernelGlobalObject kernelGlobalObject);
}
