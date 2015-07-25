package banjo.expr.free;

import banjo.eval.Environment;
import banjo.eval.expr.FreeSlotInstance;
import banjo.eval.expr.ObjectInstance;
import banjo.eval.expr.RecursiveSlotInstance;
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
import fj.data.TreeMap;

public class FreeObjectLiteral implements FreeExpression {
	public final List<SourceFileRange> ranges;
	public final List<P3<Identifier, Option<Identifier>, FreeExpression>> slots;

    public FreeObjectLiteral(List<SourceFileRange> ranges,
            List<P3<Identifier, Option<Identifier>, FreeExpression>> slots) {
		this.ranges = ranges;
		this.slots = slots;
    }
	@Override
	public Value apply(Environment env) {
		List<P2<String,SlotInstance>> _slots = slots.map(s -> P.p(s._1().id,
				 s._2().isSome() ?
					(SlotInstance)new RecursiveSlotInstance(s._1(), s._2().some(), s._3(), env) :
						(SlotInstance)new FreeSlotInstance(s._3().apply(env))));
		return new SlotMemoizer(new ObjectInstance(ranges, TreeMap.treeMap(Ord.stringOrd, _slots)));
	}

	@Override
	public String toString() {
	    return "{" + ListUtil.insertCommas(slots.map(p-> p._1().id+"=...")) + "}";
	}
}