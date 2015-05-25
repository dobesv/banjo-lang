package banjo.expr.token;

import banjo.expr.BadExpr;
import banjo.expr.core.CoreExprAlgebra;
import banjo.expr.core.CoreExprVisitor;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.SourceExprAlgebra;
import banjo.expr.source.SourceExprVisitor;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;

public class BadIdentifier extends Identifier implements BadExpr, SourceExpr {
	public static final Ord<BadIdentifier> ORD = Ord.chain(
				Ord.stringOrd.comap((BadIdentifier x) -> x.message),
				Ord.stringOrd.comap((BadIdentifier x) -> x.originalSource)
	);
	public final String message;
	public final String originalSource;

	public BadIdentifier(List<SourceFileRange> ranges, String message, String originalSource) {
		super(ranges, originalSource);
		this.message = message;
		this.originalSource = originalSource;
	}

	public BadIdentifier(SourceExpr source) {
		this(source.getSourceFileRanges(), "Expected identifier; got "+source.toSource(), source.toSource());
	}

	public BadIdentifier(SourceFileRange sfr, String message, String originalSource) {
		this(List.single(sfr), message, originalSource);
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
		return visitor.badExpr(getSourceFileRanges(), message);
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		return visitor.badExpr(getSourceFileRanges(), message);
	}

}
