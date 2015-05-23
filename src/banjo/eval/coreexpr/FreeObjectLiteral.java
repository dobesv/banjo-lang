package banjo.eval.coreexpr;

import banjo.dom.token.Identifier;
import banjo.eval.util.MemoizingSupplier;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;
import fj.P3;
import fj.data.List;
import fj.data.Option;

public class FreeObjectLiteral implements FreeExpression {
	public final List<SourceFileRange> ranges;
	public final List<P3<Identifier, Option<Identifier>, FreeExpression>> slots;

    public FreeObjectLiteral(List<SourceFileRange> ranges,
            List<P3<Identifier, Option<Identifier>, FreeExpression>> slots) {
		this.ranges = ranges;
		this.slots = slots;
    }
	@Override
	public Object apply(Environment env) {
	    return new MemoizingSupplier<Object>(new ObjectInstanceFactory(ranges, env, slots));
	}

	@Override
	public String toString() {
	    return "{" + ListUtil.insertCommas(slots.map(p-> p._1().id+"=...")) + "}";
	}
}