package banjo.eval.expr;

import banjo.eval.util.JavaRuntimeSupport;
import banjo.eval.value.Value;

public class JavaRootEnvironment extends SingleBindingEnvironment implements Environment {
	public static final String JAVA_RUNTIME_ID = "java runtime";
	public static final JavaRootEnvironment INSTANCE = new JavaRootEnvironment();
	public JavaRootEnvironment() {
	    super(JAVA_RUNTIME_ID, BindingInstance.let(Value.fromJava(JavaRuntimeSupport.class)), EmptyEnvironment.INSTANCE);
    }


}
