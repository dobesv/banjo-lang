package banjo.eval.expr;

import banjo.eval.util.JavaRuntimeSupport;
import fj.Ord;
import fj.data.TreeMap;

public class JavaRootEnvironment extends TreeMapEnvironment implements Environment {
	public static final JavaRootEnvironment INSTANCE = new JavaRootEnvironment();
	public JavaRootEnvironment() {
	    super(TreeMap.<String,BindingInstance>empty(Ord.stringOrd).set("java runtime", BindingInstance.let(JavaRuntimeSupport.class)), EmptyEnvironment.INSTANCE);
    }


}
