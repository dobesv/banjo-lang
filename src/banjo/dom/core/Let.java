package banjo.dom.core;

import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.parser.util.SourceFileRange;
import fj.P;
import fj.P2;
import fj.data.List;

public class Let extends AbstractCoreExpr implements CoreExpr {
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
		} else if(bindings.isSingle() && bindings.head()._1().compareTo(body)==0 && bindings.head()._2() instanceof FunctionLiteral) {
			((FunctionLiteral)bindings.head()._2()).toSourceWithSelfName(sb, (Identifier)body);
		} else {
			sb.append('(');
			int start = sb.length();
			for(P2<Identifier, CoreExpr> binding : bindings) {
				if(sb.length() != start) sb.append(", ");
				binding._1().toSource(sb);
				sb.append(" = ");
				binding._2().toSource(sb, Precedence.COMMA);
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

}
