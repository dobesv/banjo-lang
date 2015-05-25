package banjo.expr.core;

import static java.util.Objects.requireNonNull;
import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.token.Identifier;
import banjo.expr.util.ListUtil;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;

public class FunctionLiteral extends AbstractCoreExpr implements CoreExpr {

	public final List<Identifier> args;
	public final CoreExpr body;
	public final Option<Identifier> sourceObjectBinding;


	private static final Ord<FunctionLiteral> _argsOrd = Identifier.LIST_ORD.comap((FunctionLiteral f) -> f.args);
	private static final Ord<FunctionLiteral> _bodyOrd = CoreExpr.coreExprOrd.comap((FunctionLiteral f) -> f.body);
	public static final Ord<FunctionLiteral> functionLiteralOrd = OrdUtil.chain(_argsOrd, _bodyOrd);

	/**
	 *
	 * @param ranges Original source code location
	 * @param selfArg Name given to the receiver of the call, if any
	 * @param name Name of the method
	 * @param argumentLists Names of the methods in the argument lists
	 * @param precondition Expression that must be true for the method to be the right method, it may refer to the arguments
	 * @param body Method body expression
	 * @param postcondition Expression that checks postconditions on the result of the body expression
	 */
	public FunctionLiteral(List<SourceFileRange> ranges, List<Identifier> args, CoreExpr body, Option<Identifier> sourceObjectBinding) {
		super(ranges.hashCode() + (31 * args.hashCode()) + (97 * body.hashCode()), ranges);
		this.args = requireNonNull(args);
		this.body = requireNonNull(body);
		this.sourceObjectBinding = requireNonNull(sourceObjectBinding);
	}

	public FunctionLiteral(List<Identifier> args, CoreExpr body) {
		this(List.nil(), args, body, Option.none());
    }

	public FunctionLiteral(Identifier arg, CoreExpr body) {
		this(List.nil(), List.single(arg), body, Option.none());
    }

	@Override
	public String toString() {
		if(isSelector()) {
			return "." + body.acceptVisitor(new BaseCoreExprVisitor<String>() {
				@Override
				public String fallback() {
					throw new IllegalStateException();
				}

				@Override
				public String identifier(Identifier n) {
				    return "";
				}
				@Override
				public String call(Call n) {
					return n.target.acceptVisitor(this) + n.argsToString();
				}

				@Override
				public String slotReference(SlotReference slotReference) {
					return slotReference.object.acceptVisitor(this) +
							slotReference.slotName.toString();
				}
			});
		} else {
			P2<Option<Identifier>, CoreExpr> selfSplit = checkRecursiveBinding();
			return selfSplit._1().map(x -> x.toString()).orSome("")+"("+ListUtil.insertCommas(args)+") ↦ "+selfSplit._2().toString();
		}
	}

	@Override
	public void toSource(final StringBuffer sb) {
		if(isSelector()) {
			sb.append('.');
			body.acceptVisitor(new BaseCoreExprVisitor<Void>() {
				@Override
				public Void fallback() {
					throw new IllegalStateException();
				}

				@Override
				public Void identifier(Identifier n) {
					assert n.eql(args.head());
				    return null;
				}
				@Override
				public Void call(Call n) {
					n.target.acceptVisitor(this);
					n.argsToSource(sb);
					return null;
				}

				@Override
				public Void slotReference(SlotReference slotReference) {
					slotReference.object.acceptVisitor(this);
					slotReference.slotName.toSource(sb);
				    return null;
				}
			});
		} else {
			P2<Option<Identifier>, CoreExpr> selfSplit = checkRecursiveBinding();
			selfSplit._1().forEach(x -> x.toSource(sb));
			sb.append('(');
			ListUtil.insertCommas(sb, args, a -> a.toSource(sb, Precedence.COMMA));
			sb.append(") ↦ ");
			selfSplit._2().toSource(sb, Operator.FUNCTION.getRightPrecedence());
		}
	}

	public P2<Option<Identifier>, CoreExpr> checkRecursiveBinding() {
		return P.p(sourceObjectBinding, body);
	}

	public CoreExpr getBody() {
		return this.body;
	}

	@Override
	public Precedence getPrecedence() {
		return Operator.FUNCTION.getPrecedence();
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.functionLiteral(this);
	}

	@Override
	public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
		return visitor.functionLiteral(getSourceFileRanges(), args, body.acceptVisitor(visitor), sourceObjectBinding);
	}

	public static FunctionLiteral function(Identifier arg, CoreExpr body) {
		return new FunctionLiteral(SourceFileRange.EMPTY_LIST, List.single(arg), body, Option.none());
	}

	public FunctionLiteral withBody(CoreExpr body) {
		if(body == this.body)
			return this;
	    return new FunctionLiteral(getSourceFileRanges(), args, body, Option.none());
    }

	/**
	 * True if this function literal follows the form:
	 *
	 * <code> _it -> _it.something</code>
	 */
	public boolean isSelector() {
		if(!args.isSingle() || sourceObjectBinding.isSome())
			return false;
		return args.map(a ->
			body.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
				@Override
				public Boolean call(Call n) {
					return n.target.eql(args.head()) || n.target.acceptVisitor(this);
				}

				public Boolean slotReference(SlotReference slotReference) {
					return slotReference.object.eql(args.head()) || slotReference.object.acceptVisitor(this);
				}

				@Override
				public Boolean fallback() {
				    return false;
				}
			})
		).orHead(P.p(Boolean.FALSE));
	}

	public static final Identifier _it = new Identifier("_it");
	public static FunctionLiteral selector(Identifier name, CoreExpr ... args) {
		return new FunctionLiteral(List.single(_it), Call.slot(_it, name, List.list(args)));
	}

	public static FunctionLiteral selector(String name, CoreExpr ... args) {
	    return selector(new Identifier(name), args);
    }

	public void toSourceWithSelfName(StringBuffer sb, Identifier selfName) {
		selfName.toSource(sb);
		toSource(sb);
    }

}