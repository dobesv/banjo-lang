package banjo.dom.core;

import fj.P;
import fj.Unit;
import fj.data.List;
import fj.data.Option;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.parser.util.SourceFileRange;

public class SlotReference extends AbstractCoreExpr implements CoreExpr {
	public final CoreExpr object;
	public final Identifier slotName;

	public SlotReference(List<SourceFileRange> sourceFileRanges,
            CoreExpr object, Identifier slotName) {
	    super(sourceFileRanges.hashCode() + object.hashCode() + slotName.hashCode(), sourceFileRanges);
	    this.object = object;
	    this.slotName = slotName;
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
			Operator.PROJECTION.toSource(sb);
			slotName.toSource(sb);
			return Unit.unit();
		}));
	}

	@Override
	public Precedence getPrecedence() {
		return Operator.PROJECTION.getPrecedence();
	}

	@Override
    public boolean equals(Object obj) {
	    if (this == obj)
		    return true;
	    if (!super.equals(obj))
		    return false;
	    if (!(obj instanceof SlotReference))
		    return false;
	    SlotReference other = (SlotReference) obj;
	    if (object == null) {
		    if (other.object != null)
			    return false;
	    } else if (!object.equals(other.object))
		    return false;
	    if (slotName == null) {
		    if (other.slotName != null)
			    return false;
	    } else if (!slotName.equals(other.slotName))
		    return false;
	    return true;
    }

	public Option<Operator> getBinaryOperator() {
	    return Option.fromNull(Operator.fromMethodName(slotName, true));
	}

	public Option<Operator> getUnaryOperator() {
	    return Option.fromNull(Operator.fromMethodName(slotName, false));
    }


}
