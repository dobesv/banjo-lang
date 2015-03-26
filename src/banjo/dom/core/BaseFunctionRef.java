package banjo.dom.core;

import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.data.List;

/**
 * A primitive operation for reading the metadata of an object.
 *
 * The metadata is a map from a method name to that method's metadata.
 */
public class BaseFunctionRef extends AbstractCoreExpr implements CoreExpr {
	public static final Ord<BaseFunctionRef> ORD = Identifier.ORD.comap((x) -> x.name);
	public final Identifier name;

	public BaseFunctionRef(List<SourceFileRange> sourceFileRanges, Identifier name) {
	    super(name.hashCode(), sourceFileRanges);
	    this.name = name;
    }

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
	    return visitor.baseFunctionRef(getSourceFileRanges(), name);
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
	    return visitor.baseFunctionRef(this);
	}

	@Override
    public void toSource(StringBuffer sb) {
		Operator.BASE_FUNCTION.toSource(sb);
		name.toSource(sb);
    }

	@Override
    public Precedence getPrecedence() {
	    return Operator.BASE_FUNCTION.precedence;
    }

}
