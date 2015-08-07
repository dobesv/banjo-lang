package banjo.expr.core;

import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.token.Identifier;
import banjo.expr.util.ListUtil;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.data.List;
import fj.data.Option;

public class Call extends AbstractCoreExpr implements CoreExpr {
	public static final Ord<Call> callOrd = OrdUtil.chain(
		CoreExpr.coreExprOrd.contramap(call -> call.target),
		CoreExpr.listOfCoreExprOrd.contramap(call -> call.args)
	);
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
		getBinaryOperator().map(op -> {
			CoreExpr first;
			CoreExpr second;
			if(target instanceof Call) {
				first = ((Call)target).args.head();
				second = args.head();
			} else {
				first = ((Projection)target).object;
				second = args.head();
			}
			boolean switched = op.isSelfOnRightMethodOperator();
			(switched? second : first).toSource(sb, op.getLeftPrecedence());
			sb.append(' ');
			op.toSource(sb);
			sb.append(' ');
			(switched? first : second).toSource(sb, op.getRightPrecedence());
			return op;
		}).orSome(P.lazy(u -> {
			target.toSource(sb, Operator.CALL.getLeftPrecedence());
			argsToSource(sb);
			return Operator.CALL;
		}));
	}

	public boolean isBinaryOp() {
		return getBinaryOperator().isSome();
	}

	public Option<Operator> getBinaryOperator() {
		if(!args.isSingle())
	    	return Option.none();
	    if(target instanceof Call) {
	    	Call subcall = (Call)target;
	    	if(subcall.args.isSingle() && subcall.target instanceof Identifier) {
	    		Operator op = Operator.fromMethodName((Identifier)subcall.target, true);
	    		return Option.fromNull(op);
	    	}
	    } else if(target instanceof Projection) {
	    	Projection slotRef = (Projection)target;
			return slotRef.getBinaryOperator();
	    }
    	return Option.none();
    }
	public void argsToSource(final StringBuffer sb) {
	    sb.append('(');
		ListUtil.insertCommas(sb, args, e -> e.toSource(sb, Operator.COMMA.getPrecedence()));
		sb.append(')');
    }
	public String argsToString() {
		return "(" + ListUtil.insertCommas(args) + ")";
	}
	public String toString() {
		return getBinaryOperator().map(op ->
		   op.isSelfOnRightMethodOperator() ?
				   args.head().toString() + " " + op.getOp() + " " + ((Projection)target).object :
		   ((Projection)target).object + " " + op.getOp() + " " + args.head().toString()
	    ).orSome(target+argsToString());
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.call(this);
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
		return new Call(new Projection(target, name), args);
	}
	public static Call slot(CoreExpr target, Identifier name, CoreExpr arg) {
		return new Call(new Projection(target, name), List.single(arg));
	}
	public static Call slot(CoreExpr target, Identifier name) {
		return new Call(new Projection(target, name), List.nil());
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
