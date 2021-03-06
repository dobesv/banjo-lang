package banjo.expr.token;

import banjo.expr.BadExpr;
import banjo.expr.core.CoreExprAlgebra;
import banjo.expr.core.CoreExprVisitor;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.SourceExprAlgebra;
import banjo.expr.source.SourceExprVisitor;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;
import fj.data.Set;

public class BadIdentifier extends Identifier implements BadExpr, SourceExpr {
	public static final Ord<BadIdentifier> ORD = OrdUtil.chain(
				Ord.stringOrd.contramap((BadIdentifier x) -> x.message),
				Ord.stringOrd.contramap((BadIdentifier x) -> x.originalSource)
	);
	public final String message;
	public final String originalSource;

	public BadIdentifier(Set<SourceFileRange> ranges, String message, String originalSource) {
		super(ranges, 0, originalSource);
		this.message = message;
		this.originalSource = originalSource;
	}

	public BadIdentifier(SourceExpr source) {
		this(source.getRanges(), "Expected identifier; got "+source.toSource(), source.toSource());
	}

	public BadIdentifier(SourceFileRange sfr, String message, String originalSource) {
		this(Set.single(SourceFileRange.ORD, sfr), message, originalSource);
    }

	@Override
	public void toSource(StringBuffer sb) {
		Identifier.toSource(originalSource, sb);
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.badIdentifier(this);
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.badIdentifier(this);
	}

	@Override
	public String getMessage() {
		return this.message;
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.single(this);
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.badExpr(getRanges(), message);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.badExpr(getRanges(), message);
	}

}
