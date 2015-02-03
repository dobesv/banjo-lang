package banjo.dom.core;

import banjo.dom.token.Identifier;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.P2;
import fj.data.List;
import fj.data.Set;

public class FreeVariableGatherer implements CoreExprAlgebra<Set<Key>> {
    private static final Set<Key> LITERAL_DEPS = Set.set(Key.ORD, Identifier.DATA);
	public static final Set<Key> EMPTY_KEY_SET = Set.empty(Key.ORD);

	@Override
    public Set<Key> badExpr(List<SourceFileRange> ranges, String message,
            Object... args) {
		return EMPTY_KEY_SET;
    }

	private static Set<Key> flatten(List<Set<Key>> methods) {
	    return Set.join(Key.ORD, Set.set(Ord.setOrd(Key.ORD), methods));
    }

	@Override
    public Set<Key> objectLiteral(List<SourceFileRange> ranges,
            List<Set<Key>> methods) {
	    return flatten(methods);
    }

	@Override
    public Set<Key> numberLiteral(List<SourceFileRange> ranges, Number value,
            String suffix) {
	    return LITERAL_DEPS;
    }

	@Override
    public Set<Key> stringLiteral(List<SourceFileRange> ranges, String text) {
	    return LITERAL_DEPS;
    }

	@Override
    public Set<Key> listLiteral(List<SourceFileRange> ranges,
            List<Set<Key>> elements) {
	    return flatten(elements);
    }

	@Override
    public Set<Key> call(List<SourceFileRange> ranges, Set<Key> object,
            Set<Key> name, List<List<Set<Key>>> argumentLists,
            boolean optional, boolean callNext) {
	    return object.union(flatten(List.join(argumentLists)));
    }

	@Override
    public Set<Key> extend(List<SourceFileRange> ranges, Set<Key> base,
            Set<Key> extension) {
	    return base.union(extension);
    }

	@Override
    public Set<Key> inspect(List<SourceFileRange> ranges, Set<Key> target) {
	    return target;
    }

	@Override
    public Set<Key> method(List<SourceFileRange> ranges, Set<Key> selfArg,
            Set<Key> name, List<List<Set<Key>>> argumentLists,
            Set<Key> precondition, Set<Key> body, Set<Key> postcondition) {
		Set<Key> defs = selfArg.union(flatten(List.join(argumentLists)));
		Set<Key> refs = precondition.union(body).union(postcondition);
		return refs.minus(defs);
    }

	@Override
    public Set<Key> identifier(List<SourceFileRange> ranges, String id) {
	    return Set.set(Key.ORD, new Identifier(id));
    }

	@Override
    public Set<Key> mixfixFunctionIdentifier(
            List<SourceFileRange> sourceFileRanges, List<String> parts) {
	    return Set.set(Key.ORD, new MixfixFunctionIdentifier(parts));
    }

	@Override
    public Set<Key> anonymous() {
	    return EMPTY_KEY_SET;
    }

	@Override
    public Set<Key> let(List<SourceFileRange> sourceFileRanges,
            List<P2<Key, Set<Key>>> bindings, Set<Key> body) {
		Set<Key> defs = Set.set(Key.ORD, bindings.map(P2.__1()));
		Set<Key> refs = flatten(bindings.map(P2.__2())).union(body);
	    return refs.minus(defs);
    }

	public static Set<Key> freeVars(CoreExpr expr) {
		return expr.acceptVisitor(new FreeVariableGatherer());
	}
}
