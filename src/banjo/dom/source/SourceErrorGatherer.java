package banjo.dom.source;

import static fj.data.List.single;
import banjo.dom.BadExpr;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class SourceErrorGatherer implements SourceExprAlgebra<List<BadExpr>> {

	public static List<BadExpr> getProblems(SourceExpr sourceExpr) {
		return sourceExpr.acceptVisitor(new SourceErrorGatherer());
	}

	@Override
	public List<BadExpr> badExpr(List<SourceFileRange> sourceFileRanges,
			String messageTemplate, Object... args) {
		return single((BadExpr)new BadSourceExpr(sourceFileRanges, messageTemplate, args));
	}

	@Override
	public List<BadExpr> binaryOp(List<SourceFileRange> sourceFileRanges,
			Operator operator, List<SourceFileRange> operatorRanges, List<BadExpr> left,
			List<BadExpr> right) {
		return left.append(right);
	}

	@Override
	public List<BadExpr> unaryOp(List<SourceFileRange> sourceFileRanges,
			Operator operator, List<SourceFileRange> operatorRanges, List<BadExpr> operand) {
		return operand;
	}

	@Override
	public List<BadExpr> emptyExpr(List<SourceFileRange> sourceFileRanges) {
		return List.nil();
	}

	@Override
	public List<BadExpr> identifier(List<SourceFileRange> sourceFileRanges, String id) {
		return List.nil();
	}

	@Override
	public List<BadExpr> numberLiteral(List<SourceFileRange> sourceFileRanges, Number number, String suffix) {
		return List.nil();
	}

	@Override
	public List<BadExpr> stringLiteral(List<SourceFileRange> sourceFileRanges, String string) {
		return List.nil();
	}

}
