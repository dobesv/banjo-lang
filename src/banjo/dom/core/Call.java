package banjo.dom.core;

import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.Unit;
import fj.data.List;
import fj.data.Option;

public class Call extends AbstractCoreExpr implements CoreExpr {

	public final CoreExpr target;
	public final List<CoreExpr> args;

	public Call(List<SourceFileRange> ranges, CoreExpr target, List<CoreExpr> args) {
		super(target.hashCode() + args.hashCode(), ranges);
		this.target = target;
		this.args = args;
	}

	public Call(CoreExpr target, List<CoreExpr> args) {
		this(SourceFileRange.EMPTY_LIST, target, args);
    }
	@Override
	public Precedence getPrecedence() {
		return Operator.CALL.getPrecedence();
	}

	@Override
	public void toSource(final StringBuffer sb) {
		if(getBinaryOperator().map(op -> {
			((SlotReference)target).object.toSource(sb, op.getLeftPrecedence());
			sb.append(' ');
			op.toSource(sb);
			sb.append(' ');
			args.head().toSource(sb, op.getRightPrecedence());
			return op;
		}).orElse(P.lazy(u -> getUnaryOperator().map(op -> {
			if(op.isParen()) {
				sb.append(op.parenType.getStartChar());
				((SlotReference)target).object.toSource(sb, op.getRightPrecedence());
				sb.append(op.parenType.getEndChar());
			} else {
				op.toSource(sb);
				((SlotReference)target).object.toSource(sb, op.getRightPrecedence());
			}
			return op;
		}))).isNone()) {
			target.toSource(sb, Operator.CALL.getLeftPrecedence());
			argsToSource(sb);
		}
	}

	public boolean isBinaryOp() {
		return getBinaryOperator().isSome();
	}

	public Option<Operator> getBinaryOperator() {
	    if(!args.isSingle())
	    	return Option.none();
	    if(!(target instanceof SlotReference))
	    	return Option.none();
		SlotReference slotRef = (SlotReference)target;
		Operator op = Operator.fromMethodName(slotRef.slotName, true);
	    return Option.fromNull(op);
    }
	public Option<Operator> getUnaryOperator() {
	    if(!args.isEmpty())
	    	return Option.none();
	    if(!(target instanceof SlotReference))
	    	return Option.none();
		SlotReference slotRef = (SlotReference)target;
		Operator op = Operator.fromMethodName(slotRef.slotName, false);
	    return Option.fromNull(op);
    }
	public void argsToSource(final StringBuffer sb) {
	    sb.append('(');
		ListUtil.insertCommas(sb, args, e -> e.toSource(sb, Operator.COMMA.getPrecedence()));
		sb.append(')');
    }

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.call(this);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!(obj instanceof Call))
			return false;
		if (!super.equals(obj))
			return false;
		final Call other = (Call) obj;
		if (!this.target.equals(other.target))
			return false;
		if (!target.equals(other.target))
			return false;
		if (!args.equals(other.args))
			return false;
		return true;
	}

	@Override
	public int compareTo(Expr o) {
		if(o == null) throw new NullPointerException();
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final Call other = (Call) o;
			if(cmp == 0) cmp = this.target.compareTo(other.target);
			if(cmp == 0) cmp = Ord.listOrd(CoreExpr.ORD).compare(args, other.args).toInt();
		}
		return cmp;
	}

	@Override
	public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
		return visitor.call(
				getSourceFileRanges(),
				target.acceptVisitor(visitor),
				args.map(x -> x.acceptVisitor(visitor))
		);
	}

	public static Call slot(CoreExpr target, Identifier name, List<CoreExpr> args) {
		return new Call(new SlotReference(target, name), args);
	}
	public static Call slot(CoreExpr target, Identifier name, CoreExpr arg) {
		return new Call(new SlotReference(target, name), List.single(arg));
	}
	public static Call slot(CoreExpr target, Identifier name) {
		return new Call(new SlotReference(target, name), List.nil());
	}
	public static Call slot(CoreExpr target, String name, List<CoreExpr> args) {
		return slot(target, new Identifier(name), args);
	}
	public static Call slot(CoreExpr target, String name, CoreExpr arg) {
		return slot(target, new Identifier(name), List.single(arg));
	}

	public static CoreExpr binaryOp(CoreExpr left, Operator op, CoreExpr right) {
	    return slot(left, op.getMethodIdentifier(), right);
    }

	public static CoreExpr unaryOp(CoreExpr operand, Operator op) {
	    return slot(operand, op.getMethodIdentifier());
    }


}
