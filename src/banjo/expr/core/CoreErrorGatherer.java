package banjo.expr.core;

import banjo.expr.BadExpr;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;

public class CoreErrorGatherer implements CoreExprAlgebra<List<BadExpr>> {
	public static final CoreErrorGatherer INSTANCE = new CoreErrorGatherer();
	public static List<BadExpr> problems(CoreExpr e) {
		return e.acceptVisitor(INSTANCE);
	}

	@Override
	public List<BadExpr> badExpr(Set<SourceFileRange> ranges, String message, Object... args) {
		return List.single(new BadCoreExpr(ranges, message, args));
	}

	@Override
	public List<BadExpr> objectLiteral(Set<SourceFileRange> ranges,
	        List<P3<Identifier, Option<Identifier>, List<BadExpr>>> slots) {
		return List.join(slots.map(
				p -> p._1().acceptVisitor(this)
					.append(p._2().map(i -> i.acceptVisitor(this)).orSome(List.nil()))
					.append(p._3())
		));
	}

	@Override
	public List<BadExpr> stringLiteral(Set<SourceFileRange> ranges, String text) {
		return List.nil();
	}

	@Override
	public List<BadExpr> listLiteral(Set<SourceFileRange> ranges, List<List<BadExpr>> elements) {
		return List.join(elements);
	}

	@Override
	public List<BadExpr> extend(Set<SourceFileRange> ranges,
			List<BadExpr> base, List<BadExpr> extension) {
		return base.append(extension);
	}

	@Override
	public List<BadExpr> inspect(Set<SourceFileRange> ranges, List<BadExpr> target) {
		return target;
	}

	@Override
	public List<BadExpr> numberLiteral(Set<SourceFileRange> ranges, Number value) {
		return List.nil();
	}

	@Override
	public List<BadExpr> identifier(Set<SourceFileRange> ranges, String op) {
		return List.nil();
	}

	@Override
	public List<BadExpr> call(Set<SourceFileRange> ranges,
	        List<BadExpr> function, List<List<BadExpr>> args) {
	    return function.append(List.join(args));
	}

	@Override
	public List<BadExpr> let(Set<SourceFileRange> sourceFileRanges,
	        List<P2<Identifier, List<BadExpr>>> bindings, List<BadExpr> body) {
		return List.join(bindings.map(p -> p._1().acceptVisitor(this).append(p._2()))).append(body);
	}

	@Override
    public List<BadExpr> functionLiteral(Set<SourceFileRange> ranges,
            List<Identifier> args, List<BadExpr> body, Option<Identifier> sourceObjectBinding) {
	    return body.append(sourceObjectBinding.map(n -> n.acceptVisitor(this)).orSome(List.nil()));
    }

	@Override
    public List<BadExpr> projection(Set<SourceFileRange> ranges,
            List<BadExpr> object, List<BadExpr> projection, boolean base) {
	    return object.append(projection);
    }

	@Override
	public List<BadExpr> baseFunctionRef(
	        Set<SourceFileRange> sourceFileRanges, Identifier name) {
	    return name.acceptVisitor(this);
	}
}
