package banjo.eval;

import banjo.dom.core.Method;
import banjo.dom.token.Key;
import fj.data.TreeMap;

public class MethodClosure {
	final Method method;
	final TreeMap<Key, EvalObject> environment;

	public MethodClosure(Method method, TreeMap<Key, EvalObject> environment) {
		super();
		this.method = method;
		this.environment = environment;
	}
	public Method getMethod() {
		return this.method;
	}
	public TreeMap<Key, EvalObject> getEnvironment() {
		return this.environment;
	}

}
