package banjo.eval.coreexpr;

import banjo.eval.util.BaseSupplier;
import banjo.eval.util.SlotMemoizer;
import banjo.expr.free.FreeExpression;
import banjo.expr.token.Identifier;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Option;
import fj.data.TreeMap;

class ObjectInstanceFactory extends BaseSupplier {
	private ObjectInstance object;

	public ObjectInstanceFactory(List<SourceFileRange> ranges,
			Environment environment,
            List<P3<Identifier, Option<Identifier>, FreeExpression>> slots) {
    }

	@Override
    public Object get() {
        return object;
    }


}