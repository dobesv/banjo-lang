package banjo.eval.expr;

import banjo.eval.util.JavaRuntimeSupport;

public class JavaRootEnvironment extends SingleBindingEnvironment implements Environment {
	public static final JavaRootEnvironment INSTANCE = new JavaRootEnvironment();
	public JavaRootEnvironment() {
	    super("java runtime", BindingInstance.let(JavaRuntimeSupport.class), EmptyEnvironment.INSTANCE);
    }


}
