package banjo.dom.core;

import fj.data.List;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.parser.util.SourceFileRange;

/**
 * If the first expression is "undefined", return the second.
 */
public class AlternativeDefinition extends AbstractCoreExpr implements CoreExpr {
	private final CoreExpr base;
	private final CoreExpr alternative;

	public AlternativeDefinition(List<SourceFileRange> sourceFileRanges, CoreExpr base,	CoreExpr alternative) {
		super(base.hashCode() + alternative.hashCode(), sourceFileRanges);
		this.base = base;
		this.alternative = alternative;
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.alternativeDefinition(this);
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.alternativeDefinition(getSourceFileRanges(), base.acceptVisitor(visitor), alternative.acceptVisitor(visitor));
	}

	@Override
	public void toSource(StringBuffer sb) {
		base.toSource(sb, Operator.IF_UNDEFINED_THEN.getPrecedence());
		Operator.IF_UNDEFINED_THEN.toSource(sb);
		alternative.toSource(sb, Operator.IF_UNDEFINED_THEN.getPrecedence());
	}

	@Override
	public Precedence getPrecedence() {
		return Operator.IF_UNDEFINED_THEN.getPrecedence();
	}

}
