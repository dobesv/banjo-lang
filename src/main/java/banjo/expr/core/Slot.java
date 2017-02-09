package banjo.expr.core;

import static java.util.Objects.requireNonNull;

import banjo.expr.source.Operator;
import banjo.expr.token.Identifier;
import banjo.expr.util.OrdUtil;
import fj.Ord;
import fj.data.List;
import fj.data.Option;

public class Slot {
	public final Identifier name;
    public final Option<Identifier> slotObjectRef;
	public final CoreExpr value;

	public Slot(Identifier name, Option<Identifier> slotObjectRef,
            CoreExpr value) {
        super();
        this.name = requireNonNull(name);
        this.slotObjectRef = requireNonNull(slotObjectRef);
        this.value = requireNonNull(value);
    }
	public Slot(Identifier name, CoreExpr value) {
		this(name, Option.none(), value);
	}

	static final Ord<Slot> ORD = OrdUtil.chain(
			Identifier.ORD.contramap(slot -> slot.name),
			OrdUtil.chain(
					Ord.optionOrd(Identifier.ORD).contramap(slot -> slot.slotObjectRef),
					CoreExprOrd.ORD.contramap(slot -> slot.value)
			));
	static final Ord<List<Slot>> LIST_ORD = Ord.listOrd(ORD);

	public boolean methodSlotToSource(StringBuffer sb) {
		if(value instanceof FunctionLiteral) {
			FunctionLiteral f = (FunctionLiteral) value;
			slotObjectRef.forEach(x -> { x.toSource(sb); sb.append('.'); });
			name.toSource(sb);
			sb.append('(');
			int start = sb.length();
			f.args.forEach(a -> { if(sb.length() > start) sb.append(", "); a.toSource(sb); });
			sb.append(") = ");
			f.body.toSource(sb, Operator.ASSIGNMENT.getRightPrecedence());
			return true;
		}
	    return false;
    }

	boolean unaryOperatorSlotToSource(StringBuffer sb) {
		Operator op = Operator.fromMethodName(name, false);
		if(op == null)
			return false;
		sb.append('(');
		if(op.isPrefix()) {
			op.toSource(sb);
			slotObjectRef.orSome(Identifier.UNDERSCORE).toSource(sb);
		} else {
			slotObjectRef.orSome(Identifier.UNDERSCORE).toSource(sb);
			op.toSource(sb);
		}
		sb.append(") = ");
		value.toSource(sb, Operator.ASSIGNMENT.getRightPrecedence());
		return true;
	}

	boolean binaryOperatorSlotToSource(StringBuffer sb) {
		Operator op = Operator.fromMethodName(name, true);
		if(op == null)
			return false;
		if(!(value instanceof FunctionLiteral))
			return false;
		FunctionLiteral f = (FunctionLiteral) value;
		if(!f.args.isSingle())
			return false;
		sb.append('(');
		if(op.isSelfOnRightMethodOperator()) {
			f.args.head().toSource(sb);
			sb.append(' ');
			op.toSource(sb);
			sb.append(' ');
			slotObjectRef.orSome(Identifier.UNDERSCORE).toSource(sb);
		} else {
			slotObjectRef.orSome(Identifier.UNDERSCORE).toSource(sb);
			sb.append(' ');
			op.toSource(sb);
			sb.append(' ');
			f.args.head().toSource(sb);
		}
		sb.append(") = ");
		f.body.toSource(sb, Operator.ASSIGNMENT.getRightPrecedence());
		return true;
	}
	public StringBuffer toSource(StringBuffer sb) {
		if(unaryOperatorSlotToSource(sb) ||
				binaryOperatorSlotToSource(sb) ||
				methodSlotToSource(sb))
			return sb;

		slotObjectRef.forEach((b) -> {
			b.toSource(sb, Operator.PROJECTION.precedence);
			Operator.PROJECTION.toSource(sb);
		});
		name.toSource(sb);
		if(!name.eql(value)) {
			sb.append(" = ");
			value.toSource(sb, Operator.ASSIGNMENT.getRightPrecedence());
		}
		return sb;
	}

	public Slot withName(Identifier newName) {
	    return new Slot(newName, slotObjectRef, value);
    }

	@Override
	public String toString() {
	    return toSource(new StringBuffer()).toString();
	}
	
    public Identifier getName() {
        return name;
    }

    public Option<Identifier> getSourceObjectBinding() {
        return slotObjectRef;
    }

    public CoreExpr getValue() {
        return value;
    }

}