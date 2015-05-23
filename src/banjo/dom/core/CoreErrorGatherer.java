package banjo.dom.core;

import banjo.dom.BadExpr;
import banjo.dom.token.Identifier;
import banjo.parser.util.SourceFileRange;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Option;

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
	        List<P3<Identifier, Option<Identifier>, List<BadExpr>>> slots) {
		return List.join(slots.map(
				p -> p._1().acceptVisitor(this)
					.append(p._2().map(i -> i.acceptVisitor(this)).orSome(List.nil()))
					.append(p._3())
		));
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
	public List<BadExpr> numberLiteral(List<SourceFileRange> ranges, Number value) {
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
		return List.join(bindings.map(p -> p._1().acceptVisitor(this).append(p._2()))).append(body);
	}

	@Override
    public List<BadExpr> functionLiteral(List<SourceFileRange> ranges,
            List<Identifier> args, List<BadExpr> body, Option<Identifier> sourceObjectBinding) {
	    return body.append(sourceObjectBinding.map(n -> n.acceptVisitor(this)).orSome(List.nil()));
    }

	@Override
    public List<BadExpr> slotReference(List<SourceFileRange> ranges,
            List<BadExpr> object, Identifier slotName, boolean base) {
	    return object.append(slotName.acceptVisitor(this));
    }

	@Override
	public List<BadExpr> baseFunctionRef(
	        List<SourceFileRange> sourceFileRanges, Identifier name) {
	    return name.acceptVisitor(this);
	}
}
