package banjo.dom.core;

import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.parser.util.ExprOrd;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.data.List;

public class FunctionLiteral extends AbstractCoreExpr implements CoreExpr {

	private static final Ord<List<Identifier>> ARGS_ORD = Ord.listOrd(Identifier.ORD);
	public final List<Identifier> args;
	public final CoreExpr body;

	public static final Ord<FunctionLiteral> ORD = ExprOrd.<FunctionLiteral>exprOrd();

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
	public FunctionLiteral(List<SourceFileRange> ranges, List<Identifier> args, CoreExpr body) {
		super(ranges.hashCode() + (31 * args.hashCode()) + (97 * body.hashCode()), ranges);
		this.args = args;
		this.body = body;
	}

	public FunctionLiteral(List<Identifier> args, CoreExpr body) {
		this(List.nil(), args, body);
    }

	public FunctionLiteral(Identifier arg, CoreExpr body) {
		this(List.nil(), List.single(arg), body);
    }

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		toSource(sb);
		return sb.toString();
	}

	@Override
	public void toSource(final StringBuffer sb) {
		sb.append('(');
		int start = sb.length();
		args.forEach(a -> (sb.length() > start ? sb.append(", ") : sb).append(a.toString()));
		sb.append(") ↦ ");
		body.toSource(Operator.FUNCTION.getRightPrecedence());

	}

	public CoreExpr getBody() {
		return this.body;
	}



	@Override
	public int compareTo(Expr o) {
		if(o == null) return -1;
		int cmp = super.compareTo(o);
		if(cmp == 0) {
			FunctionLiteral other = (FunctionLiteral) o;
			if(cmp == 0) cmp = ARGS_ORD.compare(args, other.args).toInt();
			if(cmp == 0) cmp = this.body.compareTo(other.body);
		}
		return cmp;
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
		return visitor.functionLiteral(getSourceFileRanges(), args, body.acceptVisitor(visitor));
	}

	public static FunctionLiteral function(Identifier arg, CoreExpr body) {
		return new FunctionLiteral(SourceFileRange.EMPTY_LIST, List.single(arg), body);
	}

	public FunctionLiteral withBody(CoreExpr body) {
		if(body == this.body)
			return this;
	    return new FunctionLiteral(getSourceFileRanges(), args, body);
    }

	@Override
    public boolean equals(Object obj) {
	    if (this == obj)
		    return true;
	    if (!super.equals(obj))
		    return false;
	    if (!(obj instanceof FunctionLiteral))
		    return false;
	    FunctionLiteral other = (FunctionLiteral) obj;
	    if (args == null) {
		    if (other.args != null)
			    return false;
	    } else if (!args.equals(other.args))
		    return false;
	    if (body == null) {
		    if (other.body != null)
			    return false;
	    } else if (!body.equals(other.body))
		    return false;
	    return true;
    }

	/**
	 * True if this function literal follows the form:
	 *
	 * <code> x -> x.something</code>
	 */
	public boolean isSelector() {
		if(!args.isSingle())
			return false;
		return args.map(a ->
			body.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
				@Override
				public Boolean call(Call n) {
					return n.object.compareTo(args.head()) == 0;
				}

				@Override
				public Boolean fallback() {
				    return false;
				}
			})
		).orHead(P.p(Boolean.FALSE));
	}


	public static ObjectLiteral selector(Identifier name, CoreExpr ... args) {
		return new FunctionLiteral(List.single(name), new Call(name, name, List.list(args))));
	}

	public static ObjectLiteral selector(String variant, CoreExpr ... args) {
	    return selector(new Identifier(variant), args);
    }

}