package banjo.eval.util;

import java.lang.reflect.Method;

import banjo.eval.Value;
import fj.data.List;

public class OverloadedJavaMethodCaller extends Value {
	public final Object target;
	public final Method[] methods;
	public OverloadedJavaMethodCaller(Object target, Method[] methods) {
        super();
        this.target = target;
        this.methods = methods;
    }

	@Override
	public Object call(Object recurse, Object prevImpl, List<Object> arguments) {
		return JavaRuntimeSupport.callJavaMethod(target, methods, arguments);
	}

	@Override
	public String toStringFallback() {
	    return methods[0].getDeclaringClass().getName()+"."+methods[0].getName();
	}
}