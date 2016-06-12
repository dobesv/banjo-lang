package banjo.expr.core;

import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.token.Identifier;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.Option;
import fj.data.Set;

public class Projection extends AbstractCoreExpr {
	public static final Ord<Projection> ORD = OrdUtil.chain(
			coreExprOrd.contramap(p -> p.object),
			coreExprOrd.contramap(p -> p.projection),
			Ord.booleanOrd.contramap(p -> p.base)
	);
	
    // public static final Projection LANGUAGE_KERNEL_NUMBER = new
    // Projection(Identifier.PROJECT_ROOT, Identifier.LANGUAGE_KERNEL_NUMBER);
    // public static final Projection LANGUAGE_KERNEL_STRING = new
    // Projection(Identifier.PROJECT_ROOT, Identifier.LANGUAGE_KERNEL_STRING);
    // public static final Projection PROJECT_ROOT_TRUE = new
    // Projection(Identifier.PROJECT_ROOT, Identifier.TRUE);
    // public static final Projection PROJECT_ROOT_FALSE = new
    // Projection(Identifier.PROJECT_ROOT, Identifier.FALSE);
    // public static final Projection EMPTY_LIST = new
    // Projection(Identifier.PROJECT_ROOT, Identifier.EMPTY_LIST);
    // public static final Projection SINGLE_ELEMENT_LIST = new
    // Projection(Identifier.PROJECT_ROOT, Identifier.SINGLE_ELEMENT_LIST);
    // public static final Projection FUNCTION_TRAIT = new
    // Projection(Identifier.PROJECT_ROOT, Identifier.FUNCTION_TRAIT);

	public final CoreExpr object;
	public final CoreExpr projection;
	public final boolean base;

	public Projection(Set<SourceFileRange> sourceFileRanges, CoreExpr object, CoreExpr projection, boolean base) {
		super(object.hashCode() ^ projection.hashCode() ^ (base ? 13 : 7), sourceFileRanges);
		this.object = object;
		this.projection = projection;
		this.base = base;
	}
	public Projection(Set<SourceFileRange> sourceFileRanges, CoreExpr object, CoreExpr projection) {
		this(sourceFileRanges, object, projection, false);
	}
	public Projection(CoreExpr object, CoreExpr projection) {
		this(SourceFileRange.EMPTY_SET, object, projection, false);
	}


	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.projection(this);
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.projection(getRanges(), 
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
		return new Projection(SourceFileRange.EMPTY_SET, selfBinding, name, true);
	}
}
