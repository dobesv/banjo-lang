package banjo.dom.core;

import banjo.dom.token.Identifier;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.P2;
import fj.data.List;
import fj.data.Set;

public class FreeVariableGatherer implements CoreExprAlgebra<Set<Identifier>> {
    private static final Set<Identifier> LITERAL_DEPS = Set.set(Identifier.ORD, Identifier.DATA);
	public static final Set<Identifier> EMPTY_KEY_SET = Set.empty(Identifier.ORD);

	@Override
    public Set<Identifier> badExpr(List<SourceFileRange> ranges, String message,
            Object... args) {
		return EMPTY_KEY_SET;
    }

	private static Set<Identifier> flatten(List<Set<Identifier>> methods) {
	    return Set.join(Identifier.ORD, Set.set(Ord.setOrd(Identifier.ORD), methods));
    }

	@Override
    public Set<Identifier> objectLiteral(List<SourceFileRange> ranges,
            List<P2<Identifier, Set<Identifier>>> methods) {
	    return flatten(methods.map(P2.__2()));
    }

	@Override
    public Set<Identifier> numberLiteral(List<SourceFileRange> ranges, Number value,
            String suffix) {
	    return LITERAL_DEPS;
    }

	@Override
    public Set<Identifier> stringLiteral(List<SourceFileRange> ranges, String text) {
	    return LITERAL_DEPS;
    }

	@Override
    public Set<Identifier> listLiteral(List<SourceFileRange> ranges,
            List<Set<Identifier>> elements) {
	    return flatten(elements);
    }

	@Override
    public Set<Identifier> call(List<SourceFileRange> ranges, Set<Identifier> object, List<Set<Identifier>> args) {
	    return object.union(flatten(args));
    }

	@Override
    public Set<Identifier> extend(List<SourceFileRange> ranges, Set<Identifier> base,
            Set<Identifier> extension) {
	    return base.union(extension);
    }

	@Override
    public Set<Identifier> inspect(List<SourceFileRange> ranges, Set<Identifier> target) {
	    return target;
    }

	@Override
    public Set<Identifier> identifier(List<SourceFileRange> ranges, String id) {
	    return Set.set(Identifier.ORD, new Identifier(id));
    }

	@Override
    public Set<Identifier> let(List<SourceFileRange> sourceFileRanges,
            List<P2<Identifier, Set<Identifier>>> bindings, Set<Identifier> body) {
		Set<Identifier> defs = Set.set(Identifier.ORD, bindings.map(P2.__1()));
		Set<Identifier> refs = flatten(bindings.map(P2.__2())).union(body);
	    return refs.minus(defs);
    }

	public static Set<Identifier> freeVars(CoreExpr expr) {
		return expr.acceptVisitor(new FreeVariableGatherer());
	}

	@Override
    public Set<Identifier> functionLiteral(List<SourceFileRange> ranges,
    		List<Identifier> args, Set<Identifier> body) {
	    final Set<Identifier> defs = Set.set(Identifier.ORD, args);
		Set<Identifier> refs = body;
		return refs.minus(defs);
    }

	@Override
    public Set<Identifier> slotReference(List<SourceFileRange> ranges,
            Set<Identifier> object, Identifier slotName) {
	    return object;
    }
}
