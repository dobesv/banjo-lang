package banjo.expr.free;

import banjo.eval.Environment;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.ListUtil;
import banjo.value.Value;
import fj.data.List;

public class FreeJavaHelperCall implements FreeExpression {
	public final String name;
	public final List<Value> args;
	public FreeJavaHelperCall(String name, List<Value> args) {
        super();
        this.name = name;
        this.args = args;
    }
	@Override
	public Value apply(Environment env) {
	    final Value javaHelpers = FreeExpressionFactory.javaHelpers(env);
		return javaHelpers.callMethod(name, args);
	}

	@Override
	public String toString() {
		if(name.equals("string")) return StringLiteral.toSource(args.head().toString());
	    return "java."+name+"("+ListUtil.insertCommas(args)+")";
	}

}