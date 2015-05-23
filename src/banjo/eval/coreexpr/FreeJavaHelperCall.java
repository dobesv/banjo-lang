package banjo.eval.coreexpr;

import banjo.dom.token.StringLiteral;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.parser.util.ListUtil;
import fj.data.List;

public class FreeJavaHelperCall implements FreeExpression {
	public final String name;
	public final List<Object> args;
	public FreeJavaHelperCall(String name, List<Object> args) {
        super();
        this.name = name;
        this.args = args;
    }
	@Override
	public Object apply(Environment env) {
	    return JavaRuntimeSupport.callMethod(FreeExpressionFactory.javaHelpers(env), name, args);
	}

	@Override
	public String toString() {
		if(name.equals("string")) return StringLiteral.toSource(args.head().toString());
	    return "java."+name+"("+ListUtil.insertCommas(args)+")";
	}

}