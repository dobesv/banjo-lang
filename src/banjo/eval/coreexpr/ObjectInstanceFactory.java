package banjo.eval.coreexpr;

import java.util.function.BiFunction;
import java.util.function.Supplier;

import banjo.dom.token.Identifier;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Option;
import fj.data.TreeMap;

class ObjectInstanceFactory implements Supplier<Object> {
	private ObjectInstance object;

	public ObjectInstanceFactory(List<SourceFileRange> ranges,
			Environment environment,
            List<P3<Identifier, Option<Identifier>, FreeExpression>> slots) {
		List<P2<String,SlotInstance>> _slots = slots.map(s -> P.p(s._1().id,
				 s._2().isSome() ?
					(SlotInstance)new RecursiveSlotInstance(s._1(), s._2().some(), s._3(), environment) :
						(SlotInstance)new FreeSlotInstance(() -> s._3().apply(environment))));
		this.object = new ObjectInstance(TreeMap.treeMap(Ord.stringOrd, _slots));
    }

	@Override
    public Object get() {
        return object;
    }

	@Override
	public String toString() {
		return String.valueOf(JavaRuntimeSupport.force(get()));
	}

}