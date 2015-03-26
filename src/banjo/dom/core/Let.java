package banjo.dom.core;

import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;

public class Let extends AbstractCoreExpr implements CoreExpr {

	public static final Ord<List<P2<Identifier, CoreExpr>>> BINDINGS_ORD = Ord.listOrd(Ord.p2Ord(Identifier.ORD, CoreExpr.coreExprOrd));
	public static final Ord<Let> ORD = Ord.chain(
			BINDINGS_ORD.comap(let -> let.bindings),
			coreExprOrd.comap(let -> let.body)
	);
	public final List<P2<Identifier, CoreExpr>> bindings;
	public final CoreExpr body;

	public Let(List<SourceFileRange> sourceFileRanges,
            List<P2<Identifier, CoreExpr>> bindings, CoreExpr body) {
	    super(sourceFileRanges.hashCode() + bindings.hashCode() + body.hashCode(), sourceFileRanges);
	    this.bindings = bindings;
	    this.body = body;
    }

	public Let(List<P2<Identifier, CoreExpr>> bindings, CoreExpr body) {
		this(SourceFileRange.EMPTY_LIST, bindings, body);
	}

	@Override
	public void toSource(StringBuffer sb) {
		if(bindings.isEmpty()) {
			body.toSource(sb);
		} else if(bindings.isSingle() && bindings.head()._1().eql(body) && bindings.head()._2() instanceof FunctionLiteral) {
			((FunctionLiteral)bindings.head()._2()).toSourceWithSelfName(sb, (Identifier)body);
		} else {
			sb.append('(');
			int start = sb.length();
			for(P2<Identifier, CoreExpr> binding : bindings) {
				if(sb.length() != start) sb.append(", ");
				final Identifier name = binding._1();
				final CoreExpr value = binding._2();
				// Local function def ?
				name.toSource(sb);
				if(value instanceof FunctionLiteral &&
						((FunctionLiteral)value).recursiveBindingName.map(name::equals).orSome(false)) {
					FunctionLiteral func = (FunctionLiteral)value;
					sb.append('(');
					ListUtil.insertCommas(sb, func.args, a -> a.toSource(sb, Precedence.COMMA));
					sb.append(") = ");
					func.body.toSource(sb, Operator.FUNCTION.getRightPrecedence());
				} else {
					sb.append(" = ");
					value.toSource(sb, Precedence.COMMA);
				}
			}
			sb.append(") => ");
			body.toSource(sb, Operator.LET.getRightPrecedence());
		}
	}

	@Override
	public Precedence getPrecedence() {
		return Operator.LET.getPrecedence();
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.let(this);
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.let(getSourceFileRanges(), bindings.map(p -> P.p(p._1(), p._2().acceptVisitor(visitor))), body.acceptVisitor(visitor));
	}

	public static Let single(Identifier name, CoreExpr value,
            CoreExpr body) {
	    return new Let(List.single(P.p(name, value)), body);
    }

	public CoreExpr plus(List<P2<Identifier, CoreExpr>> newBindings) {
	    return new Let(getSourceFileRanges(), newBindings.append(bindings), body);
    }

	@Override
	public String toString() {
	    return "("+ListUtil.insertCommas(bindings.map(P2.__1()).map(s -> s+" = ..."))+") "+Operator.LET.getOp()+" "+body;
	}

	/**
	 * Create a Let if bindings is not empty; else, return the body unchanged.
	 */
	public static CoreExpr let(final List<P2<Identifier, CoreExpr>> bindings, CoreExpr body) {
	    if(bindings.isEmpty())
			return body;
		return new Let(bindings, body);
	}

}
