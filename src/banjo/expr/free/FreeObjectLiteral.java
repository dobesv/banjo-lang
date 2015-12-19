package banjo.expr.free;

import banjo.eval.environment.Environment;
import banjo.eval.expr.ObjectLiteralInstance;
import banjo.eval.expr.SlotInstance;
import banjo.expr.token.Identifier;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import banjo.value.meta.SlotMemoizer;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public class FreeObjectLiteral implements FreeExpression {
	public final Set<SourceFileRange> ranges;
	public final List<P3<Identifier, Option<Identifier>, FreeExpression>> slots;

    public FreeObjectLiteral(Set<SourceFileRange> ranges,
            List<P3<Identifier, Option<Identifier>, FreeExpression>> slots) {
		this.ranges = ranges;
		this.slots = slots;
    }
	@Override
	public Value apply(Environment env) {
        List<P2<String, SlotInstance>> _slots = slots.map(s -> P.p(s._1().id, SlotInstance.fromFreeExpression(s._1(), s._2(), s._3(), env)));
		return new SlotMemoizer(new ObjectLiteralInstance(ranges, TreeMap.treeMap(Ord.stringOrd, _slots)));
	}

	@Override
	public String toString() {
	    return "{" + ListUtil.insertCommas(slots.map(p-> p._1().id+"=...")) + "}";
	}
}