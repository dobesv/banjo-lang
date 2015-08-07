package banjo.expr.core;

import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.token.Identifier;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.List;
import fj.data.Option;

public class Projection extends AbstractCoreExpr {
	public static final Ord<Projection> ORD = OrdUtil.chain(
			coreExprOrd.contramap(p -> p.object),
			coreExprOrd.contramap(p -> p.projection),
			Ord.booleanOrd.contramap(p -> p.base)
	);
	
	public final CoreExpr object;
	public final CoreExpr projection;
	public final boolean base;

	public Projection(List<SourceFileRange> sourceFileRanges, CoreExpr object, CoreExpr projection, boolean base) {
		super(object.hashCode() ^ projection.hashCode() ^ (base ? 13 : 7), sourceFileRanges);
		this.object = object;
		this.projection = projection;
		this.base = base;
	}
	public Projection(List<SourceFileRange> sourceFileRanges, CoreExpr object, CoreExpr projection) {
		this(sourceFileRanges, object, projection, false);
	}
	public Projection(CoreExpr object, CoreExpr projection) {
		this(SourceFileRange.EMPTY_LIST, object, projection, false);
	}


	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.projection(this);
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.projection(getSourceFileRanges(), 
				object.acceptVisitor(visitor),
				projection.acceptVisitor(visitor), // Truly this should probably be a different visitor?
				base
		);
	}

	@Override
	public void toSource(StringBuffer sb) {
		Option<Operator> unaryOperatorOption = getUnaryOperator();
		
		if(unaryOperatorOption.isSome()) {
			Operator unaryOperator = unaryOperatorOption.some();
			if(unaryOperator.isParen()) { // e.g. |x|
				sb.append(unaryOperator.getParenType().getStartChar());
				object.toSource(sb, unaryOperator.getRightPrecedence());
				sb.append(unaryOperator.getParenType().getEndChar());
			} else if(unaryOperator.isPrefix()) {
				unaryOperator.toSource(sb);
				object.toSource(sb, unaryOperator.getRightPrecedence());
			} else {
				object.toSource(sb, unaryOperator.leftPrecedence);
				unaryOperator.toSource(sb);
			}
		} else {
			Operator operator = getProjectionOperator();
			object.toSource(sb, operator.leftPrecedence);
			operator.toSource(sb);
			projection.toSource(sb, operator.getRightPrecedence());
		}
	}
	public Operator getProjectionOperator() {
		return base ? Operator.BASE_SLOT : Operator.PROJECTION;
	}

	public Option<Operator> getUnaryOperator() {
		return projection.acceptVisitor(new BaseCoreExprVisitor<Option<Operator>>() {
			@Override
			public Option<Operator> identifier(Identifier n) {
				String slotName = n.id;
			    return Option.fromNull(Operator.fromMethodName(slotName, false));
			}
			
			@Override
			public Option<Operator> fallback() {
				return Option.none();
			}
		});
	}

	@Override
	public Precedence getPrecedence() {
		return getUnaryOperator().map(op -> op.getPrecedence()).orSome(getProjectionOperator().getPrecedence());
	}
	
	public Option<Operator> getBinaryOperator() {
		if(base) return Option.none();
		
		return projection.acceptVisitor(new BaseCoreExprVisitor<Option<Operator>>() {
			@Override
			public Option<Operator> identifier(Identifier n) {
				String slotName = n.id;
			    return Option.fromNull(Operator.fromMethodName(slotName, true));
			}
			
			@Override
			public Option<Operator> fallback() {
				return Option.none();
			}
		});
	}
	public static CoreExpr baseSlot(Identifier selfBinding, Identifier name) {
		return new Projection(SourceFileRange.EMPTY_LIST, selfBinding, name, true);
	}
}