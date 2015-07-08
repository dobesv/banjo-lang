package banjo.expr.free;

import banjo.eval.expr.Environment;
import banjo.eval.value.Value;
import banjo.expr.util.ListUtil;
import fj.data.List;

public class FreeListLiteral implements FreeExpression {
	public final List<FreeExpression> elements;

	public FreeListLiteral(List<FreeExpression> elements) {
        super();
        this.elements = elements;
    }

	@Override
	public Value apply(Environment env) {
	    final List<Value> elts = elements.map(e -> e.apply(env));
		final Value javaHelpers = FreeExpressionFactory.javaHelpers(env);
		return javaHelpers.callMethod("list", List.single(Value.fromJava(elts)));
	}
	@Override
	public String toString() {
	    return "["+ListUtil.insertCommas(elements)+"]";
	}
}