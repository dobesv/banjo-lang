package banjo.eval.coreexpr;

import java.util.function.Supplier;

import banjo.eval.util.BaseSupplier;
import banjo.eval.util.MemoizingSupplier;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.TreeMap;

public class LetEnvironment extends TreeMapEnvironment {
	public LetEnvironment(Environment parentEnvironment, List<P2<String, FreeExpression>> bindings) {
		super(e -> bind(bindings, e), parentEnvironment);
    }

	private static TreeMap<String, BindingInstance> bind(List<P2<String, FreeExpression>> bindings, Environment recursiveEnv) {
	    return TreeMap.treeMap(Ord.stringOrd, bindings.map(p -> P.p(p._1(), bindOne(p._2(), recursiveEnv))));
    }

	static class LazyBindingValue extends BaseSupplier {
		public final FreeExpression f;
		public final Environment environment;
		public LazyBindingValue(FreeExpression f, Environment environment) {
	        super();
	        this.f = f;
	        this.environment = environment;
        }

		@Override
		public Object get() {
		    return f.apply(environment);
		}
	}
	private static BindingInstance bindOne(FreeExpression f, Environment recursiveEnv) {
	    final Supplier<Object> v = new LazyBindingValue(f, recursiveEnv);
		final MemoizingSupplier<Object> memo = new MemoizingSupplier<Object>(v);
		return BindingInstance.let(memo);
    }
}