package banjo.eval;

import banjo.dom.core.Method;
import fj.data.TreeMap;

public class MethodClosure {
	final Method method;
	final TreeMap<String, BanjoObject> environment;
	public MethodClosure(Method method, TreeMap<String, BanjoObject> environment) {
		super();
		this.method = method;
		this.environment = environment;
	}
	public Method getMethod() {
		return this.method;
	}
	public TreeMap<String, BanjoObject> getEnvironment() {
		return this.environment;
	}

}
