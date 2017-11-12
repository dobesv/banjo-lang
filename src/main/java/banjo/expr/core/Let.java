package banjo.expr.core;

import banjo.expr.source.BinaryOp;
import banjo.expr.source.Operator;
import banjo.expr.source.Precedence;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.UnaryOp;
import banjo.expr.token.Identifier;
import banjo.expr.util.ListUtil;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Set;

public class Let extends AbstractCoreExpr implements CoreExpr {

	public static final Ord<List<P2<Identifier, CoreExpr>>> BINDINGS_ORD = Ord.listOrd(Ord.p2Ord(Identifier.ORD, CoreExprOrd.ORD));
	public static final Ord<Let> ORD = OrdUtil.chain(
			BINDINGS_ORD.contramap(let -> let.bindings),
            CoreExprOrd.ORD.contramap(let -> let.body)
	);
	public final List<P2<Identifier, CoreExpr>> bindings;
	public final CoreExpr body;

	public Let(Set<SourceFileRange> sourceFileRanges,
            List<P2<Identifier, CoreExpr>> bindings, CoreExpr body) {
	    super(sourceFileRanges.hashCode() + bindings.hashCode() + body.hashCode(), sourceFileRanges);
	    this.bindings = bindings;
	    this.body = body;
    }

	public Let(List<P2<Identifier, CoreExpr>> bindings, CoreExpr body) {
		this(SourceFileRange.EMPTY_SET, bindings, body);
	}

    public SourceExpr bindingSourceExpr(P2<Identifier, CoreExpr> p) {
        Identifier name = p._1();
        CoreExpr definition = p._2();
        return definition.acceptVisitor(new BaseCoreExprVisitor<SourceExpr>() {
            @Override
            public SourceExpr fallback() {
                return new BinaryOp(Operator.ASSIGNMENT, name, definition.toSourceExpr());
            }
            
            @Override
            public SourceExpr objectLiteral(ObjectLiteral n) {
                if(n.isFunctionLiteral()) {
                    Slot lambda = n.slots.head();
                    boolean sameRecArg = lambda.args.headOption().option(Boolean.FALSE, (selfName) -> selfName.id.equals(name.id)).booleanValue();
                    boolean noBaseArg = lambda.args.drop(1).headOption().option(Boolean.TRUE, (baseName) -> baseName.id.equals(Identifier.UNDERSCORE.id));
                    if(sameRecArg && noBaseArg) {
                        SourceExpr argList = BinaryOp.insertOperator(Operator.COMMA, lambda.args.drop(2));
                        SourceExpr signature = new BinaryOp(Operator.CALL, name, argList);
                        return new BinaryOp(Operator.ASSIGNMENT, signature, lambda.body.toSourceExpr());
                    }
                }
                return super.objectLiteral(n);
            }
        });
    }

    @Override
    public SourceExpr toSourceExpr() {
	    if(bindings.isEmpty())
	        return body.toSourceExpr();
	    
        SourceExpr bindingsExpr = new UnaryOp(Operator.PARENS,
                BinaryOp.insertOperator(Operator.COMMA, bindings.map(this::bindingSourceExpr)));
        return new BinaryOp(Operator.LET, bindingsExpr, body.toSourceExpr());
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
		return visitor.let(getRanges(), bindings.map(p -> P.p(p._1(), p._2().acceptVisitor(visitor))), body.acceptVisitor(visitor));
	}

	public static Let single(Identifier name, CoreExpr value,
            CoreExpr body) {
	    return new Let(List.single(P.p(name, value)), body);
    }

	public CoreExpr plus(List<P2<Identifier, CoreExpr>> newBindings) {
	    return new Let(getRanges(), newBindings.append(bindings), body);
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
