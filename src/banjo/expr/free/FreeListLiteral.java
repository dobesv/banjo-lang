package banjo.expr.free;

import banjo.eval.environment.Environment;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.List;
import fj.data.Set;

public class FreeListLiteral implements FreeExpression {
	public final List<FreeExpression> elements;
    public final Set<SourceFileRange> ranges;

    public FreeListLiteral(Set<SourceFileRange> ranges, List<FreeExpression> elements) {
        super();
        this.elements = elements;
        this.ranges = ranges;
    }

	@Override
	public Value apply(Environment env) {
	    final List<Value> elts = elements.map(e -> e.apply(env));
        final Value javaHelpers = FreeExpressionFactory.javaHelpers(env, ranges);
        return javaHelpers.callMethod("list", ranges, List.single(Value.fromJava(elts)));
	}
	@Override
	public String toString() {
	    return "["+ListUtil.insertCommas(elements)+"]";
	}
}