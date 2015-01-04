package banjo.dom.source;

import banjo.parser.util.SourceFileRange;
import fj.data.List;

public interface SourceExprAlgebra<T> {

	T badExpr(List<SourceFileRange> sourceFileRanges, String messageTemplate, Object ... args);
	T binaryOp(List<SourceFileRange> sourceFileRanges, Operator operator, List<SourceFileRange> operatorRanges, T left, T right);
	T unaryOp(List<SourceFileRange> sourceFileRanges, Operator operator, List<SourceFileRange> operatorRanges, T operand);
	T emptyExpr(List<SourceFileRange> sourceFileRanges);
	T identifier(List<SourceFileRange> sourceFileRanges, String id);
	T numberLiteral(List<SourceFileRange> sourceFileRanges, Number number, String suffix);
	T stringLiteral(List<SourceFileRange> sourceFileRanges, String string);

}
