package banjo.expr.source;

import static fj.data.List.single;

import banjo.expr.BadExpr;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

public class SourceErrorGatherer implements SourceExprAlgebra<List<BadExpr>> {

	public static List<BadExpr> getProblems(SourceExpr sourceExpr) {
		return sourceExpr.acceptVisitor(new SourceErrorGatherer());
	}

	@Override
	public List<BadExpr> badExpr(Set<SourceFileRange> sourceFileRanges,
			String messageTemplate, Object... args) {
		return single((BadExpr)new BadSourceExpr(sourceFileRanges, messageTemplate, args));
	}

	@Override
	public List<BadExpr> binaryOp(Set<SourceFileRange> sourceFileRanges,
			Operator operator, Set<SourceFileRange> operatorRanges, List<BadExpr> left,
			List<BadExpr> right) {
		return left.append(right);
	}

	@Override
	public List<BadExpr> unaryOp(Set<SourceFileRange> sourceFileRanges,
			Operator operator, Set<SourceFileRange> operatorRanges, List<BadExpr> operand) {
		return operand;
	}

	@Override
	public List<BadExpr> emptyExpr(Set<SourceFileRange> sourceFileRanges) {
		return List.nil();
	}

	@Override
	public List<BadExpr> identifier(Set<SourceFileRange> sourceFileRanges, String id) {
		return List.nil();
	}

	@Override
	public List<BadExpr> numberLiteral(Set<SourceFileRange> sourceFileRanges, Number number) {
		return List.nil();
	}

	@Override
	public List<BadExpr> stringLiteral(Set<SourceFileRange> sourceFileRanges, String string) {
		return List.nil();
	}

}
