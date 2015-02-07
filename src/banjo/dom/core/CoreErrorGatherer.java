package banjo.dom.core;

import banjo.dom.BadExpr;
import banjo.dom.token.Identifier;
import banjo.parser.util.SourceFileRange;
import fj.P2;
import fj.data.List;

public class CoreErrorGatherer implements CoreExprAlgebra<List<BadExpr>> {
	public static final CoreErrorGatherer INSTANCE = new CoreErrorGatherer();
	public static List<BadExpr> problems(CoreExpr e) {
		return e.acceptVisitor(INSTANCE);
	}

	@Override
	public List<BadExpr> badExpr(List<SourceFileRange> ranges, String message, Object... args) {
		return List.single(new BadCoreExpr(ranges, message, args));
	}

	@Override
	public List<BadExpr> objectLiteral(List<SourceFileRange> ranges,
	        List<P2<Identifier, List<BadExpr>>> slots) {
		return List.join(slots.map(P2.__2()));
	}

	@Override
	public List<BadExpr> stringLiteral(List<SourceFileRange> ranges, String text) {
		return List.nil();
	}

	@Override
	public List<BadExpr> listLiteral(List<SourceFileRange> ranges, List<List<BadExpr>> elements) {
		return List.join(elements);
	}

	@Override
	public List<BadExpr> extend(List<SourceFileRange> ranges,
			List<BadExpr> base, List<BadExpr> extension) {
		return base.append(extension);
	}

	@Override
	public List<BadExpr> inspect(List<SourceFileRange> ranges, List<BadExpr> target) {
		return target;
	}

	@Override
	public List<BadExpr> numberLiteral(List<SourceFileRange> ranges, Number value, String suffix) {
		return List.nil();
	}

	@Override
	public List<BadExpr> identifier(List<SourceFileRange> ranges, String op) {
		return List.nil();
	}

	@Override
	public List<BadExpr> call(List<SourceFileRange> ranges,
	        List<BadExpr> function, List<List<BadExpr>> args) {
	    return function.append(List.join(args));
	}

	@Override
	public List<BadExpr> let(List<SourceFileRange> sourceFileRanges,
	        List<P2<Identifier, List<BadExpr>>> bindings, List<BadExpr> body) {
		return List.join(bindings.map(p -> p._2())).append(body);
	}

	@Override
    public List<BadExpr> functionLiteral(List<SourceFileRange> ranges,
            List<Identifier> args, List<BadExpr> body) {
	    return body;
    }

	@Override
    public List<BadExpr> slotReference(List<SourceFileRange> ranges,
            List<BadExpr> object, Identifier slotName) {
	    return object;
    }
}
