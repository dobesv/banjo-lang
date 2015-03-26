package banjo.dom.core;

import fj.Ord;
import fj.P;
import fj.Unit;
import fj.data.List;
import fj.data.Option;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.parser.util.SourceFileRange;

public class SlotReference extends AbstractCoreExpr implements CoreExpr {
	public static final Ord<SlotReference> ORD = Ord.chain(
			CoreExpr.coreExprOrd.comap((SlotReference x) -> x.object),
			Identifier.ORD.comap((SlotReference x) -> x.slotName)
	);
	public final CoreExpr object;
	public final Identifier slotName;
	public boolean base;

	public SlotReference(List<SourceFileRange> sourceFileRanges,
            CoreExpr object, Identifier slotName, boolean base) {
	    super(sourceFileRanges.hashCode() + object.hashCode() + slotName.hashCode() + Boolean.hashCode(base), sourceFileRanges);
	    this.object = object;
	    this.slotName = slotName;
	    this.base = base;
    }

	public SlotReference(List<SourceFileRange> sourceFileRanges,
            CoreExpr object, Identifier slotName) {
		this(sourceFileRanges, object, slotName, false);
	}
	public SlotReference(CoreExpr object, Identifier slotName) {
		this(SourceFileRange.EMPTY_LIST, object, slotName);
    }

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.slotReference(this);
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.slotReference(getSourceFileRanges(), object.acceptVisitor(visitor), slotName);
	}

	@Override
	public void toSource(StringBuffer sb) {
		getUnaryOperator().map(unaryOp -> {
			if(unaryOp.isParen()) {
				sb.append(unaryOp.getParenType().getStartChar());
				object.toSource(sb, unaryOp.getLeftPrecedence());
				sb.append(unaryOp.getParenType().getEndChar());
			} else if(unaryOp.isPrefix()) {
				unaryOp.toSource(sb);
				object.toSource(sb, unaryOp.getLeftPrecedence());
			} else {
				object.toSource(sb, unaryOp.getRightPrecedence());
				unaryOp.toSource(sb);
			}
			return Unit.unit();
		}).orSome(P.lazy(u -> {
			object.toSource(sb);
			(base ? Operator.BASE_SLOT : Operator.PROJECTION).toSource(sb);
			slotName.toSource(sb);
			return Unit.unit();
		}));
	}

	@Override
	public String toString() {
		return getUnaryOperator().map(op ->
			op.isParen() ? op.getParenType().getStartChar()+object.toString()+op.getParenType().getEndChar() :
			op.isPrefix() ? op.getOp() + object.toString() :
			object.toString() + op.getOp()
		).orSome(P.lazy(u -> object.toString() + Operator.PROJECTION.getOp()+slotName.toString()));
	}

	@Override
	public Precedence getPrecedence() {
		return Operator.PROJECTION.getPrecedence();
	}

	public Option<Operator> getBinaryOperator() {
	    return Option.fromNull(Operator.fromMethodName(slotName, true));
	}

	public Option<Operator> getUnaryOperator() {
		if(base) return Option.none();
	    return Option.fromNull(Operator.fromMethodName(slotName, false));
    }


}
