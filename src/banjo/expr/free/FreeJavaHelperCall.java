package banjo.expr.free;

import banjo.eval.environment.Environment;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.ListUtil;
import banjo.expr.util.SourceFileRange;
import banjo.value.Value;
import fj.data.List;
import fj.data.Set;

public class FreeJavaHelperCall implements FreeExpression {
	public final String name;
    public final Set<SourceFileRange> ranges;
	public final List<Value> args;

    public FreeJavaHelperCall(String name, Set<SourceFileRange> ranges, List<Value> args) {
        super();
        this.name = name;
        this.ranges = ranges;
        this.args = args;
    }
	@Override
	public Value apply(Environment env) {
        final Value javaHelpers = FreeExpressionFactory.javaHelpers(env, ranges);
        return javaHelpers.callMethod(name, ranges, args);
	}

	@Override
	public String toString() {
		if(name.equals("string")) return StringLiteral.toSource(args.head().toString());
	    return "java."+name+"("+ListUtil.insertCommas(args)+")";
	}

}