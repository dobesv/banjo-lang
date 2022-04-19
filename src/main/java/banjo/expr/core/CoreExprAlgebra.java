package banjo.expr.core;

import banjo.expr.ExprAlgebra;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Set;


public interface CoreExprAlgebra<T> extends ExprAlgebra<T> {
    T numberLiteral(Set<SourceFileRange> ranges, Number value, String source);

    T kernelNumberLiteral(Set<SourceFileRange> ranges, Number value, String source);

    T stringLiteral(Set<SourceFileRange> ranges, String text);

    T kernelStringLiteral(Set<SourceFileRange> ranges, String text);

	T listLiteral(Set<SourceFileRange> ranges, List<T> elements);

	T extend(Set<SourceFileRange> ranges, T base, T extension);

	T identifier(Set<SourceFileRange> ranges, String id);

    T scoped(Set<SourceFileRange> ranges, T body, T args);

    T kernelGlobalObject(KernelGlobalObject kernelGlobalObject);

	T nil();

	T binding(Identifier name, T body);
}
