package banjo.eval;

import banjo.dom.core.Method;
import fj.data.TreeMap;

public class MethodClosure {
	final Method method;
	final TreeMap<String, EvalObject> environment;
	public MethodClosure(Method method, TreeMap<String, EvalObject> environment) {
		super();
		this.method = method;
		this.environment = environment;
	}
	public Method getMethod() {
		return this.method;
	}
	public TreeMap<String, EvalObject> getEnvironment() {
		return this.environment;
	}

}
