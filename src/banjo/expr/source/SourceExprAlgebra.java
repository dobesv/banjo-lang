package banjo.expr.source;

import banjo.expr.util.SourceFileRange;
import fj.data.Set;

public interface SourceExprAlgebra<T> {

	T badExpr(Set<SourceFileRange> sourceFileRanges, String messageTemplate, Object ... args);
	T binaryOp(Set<SourceFileRange> sourceFileRanges, Operator operator, Set<SourceFileRange> operatorRanges, T left, T right);
	T unaryOp(Set<SourceFileRange> sourceFileRanges, Operator operator, Set<SourceFileRange> operatorRanges, T operand);
	T emptyExpr(Set<SourceFileRange> sourceFileRanges);
	T identifier(Set<SourceFileRange> sourceFileRanges, String id);
	T numberLiteral(Set<SourceFileRange> sourceFileRanges, Number number);
	T stringLiteral(Set<SourceFileRange> sourceFileRanges, String string);

}
