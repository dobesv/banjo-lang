package banjo.eval;

import java.util.Arrays;
import java.util.Iterator;

import org.eclipse.jdt.annotation.NonNull;

import banjo.dom.core.Method;
import banjo.dom.core.MethodFormalArgument;
import fj.Ord;
import fj.P2;
import fj.data.TreeMap;

/**
 * An object that carries method definitions plus a captured environment.
 */
public final class BanjoObject {
	@NonNull @SuppressWarnings("null")
	public static final TreeMap<String, MethodClosure> NO_METHODS = TreeMap.empty(Ord.stringOrd);

	@NonNull @SuppressWarnings("null")
	public static final TreeMap<String,BanjoObject> EMPTY_ENVIRONMENT = TreeMap.empty(Ord.stringOrd);

	private final BanjoEvaluator evaluator;
	private final TreeMap<String, MethodClosure> methods;

	public BanjoObject(BanjoEvaluator evaluator) {
		this(NO_METHODS, evaluator);
	}

	public BanjoObject(TreeMap<String, MethodClosure> methods, BanjoEvaluator evaluator) {
		super();
		this.methods = methods;
		this.evaluator = evaluator;
	}

	public TreeMap<String, MethodClosure> getMethods() {
		return this.methods;
	}

	public BanjoObject extend(BanjoObject extension) {
		final TreeMap<String, MethodClosure> newMethods = this.methods;
		for(final P2<String,MethodClosure> p : extension.getMethods()) {
			newMethods.set(p._1(), p._2());
		}
		return new BanjoObject(newMethods, this.evaluator);
	}

	public BanjoObject call(String methodName, Iterable<BanjoObject> arguments) {
		final MethodClosure impl = this.methods.get(methodName).toNull();
		if(impl == null)
			return this.evaluator.emptyObject;
		final Method methodDef = impl.getMethod();
		@NonNull
		TreeMap<String, BanjoObject> newEnvironment = impl.getEnvironment();

		// Add arguments to the environment
		final Iterator<BanjoObject> argsIt = arguments.iterator();
		if(methodDef.hasSelfName())
			newEnvironment = newEnvironment.set(methodDef.getSelfName().getKeyString(), this);
		for(final MethodFormalArgument paramDecl : methodDef.getArguments()) {
			newEnvironment = newEnvironment.set(paramDecl.getName().getKeyString(), argsIt.next());
			if(paramDecl.hasAssertion())
				throw new Error("TODO: Assertions not implemented");
		}
		if(methodDef.hasGuarantee())
			throw new Error("TODO: Guarantee not implemented");

		// Evaluate the method body
		return this.evaluator.eval(methodDef.getBody(), newEnvironment);

	}

	@SuppressWarnings("null")
	public BanjoObject call(String methodName, BanjoObject ... arguments) {
		return call(methodName, Arrays.asList(arguments));
	}
}
