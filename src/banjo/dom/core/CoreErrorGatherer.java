package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.dom.token.BadIdentifier;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class CoreErrorGatherer implements CoreExprAlgebra<List<BadExpr>> {

	@Override
	public List<BadExpr> badExpr(List<SourceFileRange> ranges, String message, Object... args) {
		return List.<BadExpr>single(new BadCoreExpr(ranges, message, args));
	}

	@Override
	public List<BadExpr> objectLiteral(List<SourceFileRange> ranges,
			List<List<BadExpr>> methods) {
		return List.join(methods);
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
	public List<BadExpr> call(List<SourceFileRange> ranges,
			List<BadExpr> object, List<List<BadExpr>> nameParts,
			List<List<List<BadExpr>>> argumentLists) {
		return object.append(List.join(nameParts)).append(List.join(List.join(argumentLists)));
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
	public List<BadExpr> method(List<SourceFileRange> sourceFileRanges,
			List<List<BadExpr>> selfArg, List<List<BadExpr>> nameParts,
			List<List<List<List<BadExpr>>>> argumentLists, List<BadExpr> body) {
		return List.join(selfArg).append(List.join(nameParts)).append(List.join(List.join(List.join(argumentLists)))).append(body);
	}

}
