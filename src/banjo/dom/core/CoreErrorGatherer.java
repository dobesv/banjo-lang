package banjo.dom.core;

import banjo.dom.BadExpr;
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
	public List<BadExpr> method(List<SourceFileRange> ranges,
			List<BadExpr> selfArg, List<BadExpr> name,
			List<List<List<BadExpr>>> argumentLists,
			List<BadExpr> precondition, List<BadExpr> body,
			List<BadExpr> postcondition) {
		return selfArg.append(name).append(List.join(List.join(argumentLists))).append(body).append(precondition).append(postcondition);
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
	public List<BadExpr> mixfixFunctionIdentifier(
			List<SourceFileRange> sourceFileRanges, List<String> parts) {
		return List.nil();
	}

	@Override
	public List<BadExpr> anonymous() {
		return List.nil();
	}

	@Override
	public List<BadExpr> call(List<SourceFileRange> ranges,
			List<BadExpr> object, List<BadExpr> name,
			List<List<List<BadExpr>>> argumentLists, boolean optional,
			boolean callNext) {
		return object.append(name).append(List.join(List.join(argumentLists)));
	}

	@Override
	public List<BadExpr> let(List<SourceFileRange> sourceFileRanges,
	        List<P2<Key, List<BadExpr>>> bindings, List<BadExpr> body) {
		return List.join(bindings.map(p -> p._2())).append(body);
	}
}
