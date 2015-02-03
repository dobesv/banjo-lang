package banjo.dom.core;

import fj.data.List;
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
		object.toSource(sb);
		Operator.PROJECTION.toSource(sb);
		slotName.toSource(sb);
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


}
