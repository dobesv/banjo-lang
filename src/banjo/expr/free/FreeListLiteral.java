package banjo.expr.free;

import banjo.eval.coreexpr.Environment;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.expr.util.ListUtil;
import fj.data.List;

public class FreeListLiteral implements FreeExpression {
	public final List<FreeExpression> elements;

	public FreeListLiteral(List<FreeExpression> elements) {
        super();
        this.elements = elements;
    }

	@Override
	public Object apply(Environment env) {
	    final List<Object> elts = elements.map(e -> e.apply(env));
		return JavaRuntimeSupport.callMethod(FreeExpressionFactory.javaHelpers(env), "list", List.single(elts));
	}
	@Override
	public String toString() {
	    return "["+ListUtil.insertCommas(elements)+"]";
	}
}