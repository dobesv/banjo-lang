package banjo.eval;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.Method;
import banjo.dom.source.Operator;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import fj.F2;
import fj.P2;
import fj.data.List;
import fj.data.TreeMap;

/**
 * An object that carries method definitions plus a captured environment.
 */
public final class CoreExprObject {
	@NonNull @SuppressWarnings("null")
	public static final TreeMap<Key, MethodClosure> NO_METHODS = TreeMap.empty(Key.ORD);

	@NonNull @SuppressWarnings("null")
	public static final TreeMap<Key,Object> EMPTY_ENVIRONMENT = TreeMap.empty(Key.ORD);

	private final CoreExprEvaluator evaluator;
	private final TreeMap<Key, MethodClosure> methods;
	private final @Nullable Object nextObject;

	public CoreExprObject(CoreExprEvaluator evaluator) {
		this(NO_METHODS, evaluator);
	}

	public CoreExprObject(TreeMap<Key, MethodClosure> methods, CoreExprEvaluator evaluator) {
		this(methods, null, evaluator);
	}

	static public CoreExprObject nativeWrapper(Object obj, CoreExprEvaluator evaluator) {
		return new CoreExprObject(NO_METHODS, obj, evaluator);
	}

	public CoreExprObject(TreeMap<Key, MethodClosure> methods, @Nullable Object nextObject, CoreExprEvaluator evaluator) {
		super();
		this.methods = methods;
		this.nextObject = nextObject;
		this.evaluator = evaluator;
	}

	public TreeMap<Key, MethodClosure> getMethods() {
		return this.methods;
	}

	public CoreExprObject extend(Object ext) {
		if(ext instanceof CoreExprObject) {
			CoreExprObject extension = (CoreExprObject)ext;
			final TreeMap<Key, MethodClosure> newMethods = this.methods;
			for(final P2<Key,MethodClosure> p : extension.getMethods()) {
				newMethods.set(p._1(), p._2());
			}
			return new CoreExprObject(newMethods, this.evaluator);
		} else {
			return new CoreExprObject(methods, ext, evaluator);
		}
	}

	public Object call(Key key, List<List<Object>> argumentLists) throws ContractFailure {
		final MethodClosure impl = this.methods.get(key).toNull();
		if(impl == null) {
			// TODO Delegate to java object if we have one
			throw new ContractFailure("No such method: "+key);
		}

		final Method methodDef = impl.getMethod();
		@NonNull
		TreeMap<Key, Object> newEnvironment = impl.getEnvironment();

		// Add arguments to the environment
		if(methodDef.hasSelfArg())
			newEnvironment = newEnvironment.set(methodDef.getSelfArg(), this);

		newEnvironment = nonNull(methodDef.getArgumentLists().zip(argumentLists).foldRight(new F2<P2<List<Key>, List<Object>>, TreeMap<Key, Object>, TreeMap<Key, Object>>() {
			public @Nullable TreeMap<Key,Object> f(@Nullable P2<List<Key>, List<Object>> a, @Nullable TreeMap<Key, Object> b) {
				if(a == null) throw new NullPointerException();
				if(b == null) throw new NullPointerException();
				List<Object> argValues = nonNull(a._2());
				List<Key> argNames = nonNull(a._1());
				return argNames.zip(argValues).foldRight(new F2<P2<Key,Object>,TreeMap<Key,Object>, TreeMap<Key, Object>> () {
					@Override
					public TreeMap<Key, Object> f(@Nullable P2<Key, Object> aa, @Nullable TreeMap<Key,Object> bb) {
						if(aa == null || aa._1() == null || aa._2() == null) throw new NullPointerException();
						if(bb == null) throw new NullPointerException();
						return bb.set(aa._1(), aa._2());
					}
				}, b);
			}

		}, newEnvironment));

		if(methodDef.hasPrecondition()) {
			Object test = this.evaluator.eval(methodDef.getPrecondition(), newEnvironment);
			if(!EvalUtil.toBoolean(test)) {
				throw new ContractFailure("Precondition failed: "+methodDef.getPrecondition());
			}
		}
		if(methodDef.hasPostcondition())
			throw new Error("TODO: Postcondition not implemented");

		// Evaluate the method body
		return this.evaluator.eval(methodDef.getBody(), newEnvironment);

	}

	public Object call(Key methodName, Object ... arguments) throws ContractFailure {
		return call(methodName, List.single(List.<Object>list(arguments)));
	}

	public boolean hasMethod(Key name) {
		return methods.contains(name);
	}

	/**
	 * If this object behaves as a boolean in banjo, convert it to a java boolean.
	 *
	 * Behaves like:
	 *
	 *  <code>this # { true = Boolean.TRUE , false = Boolean.FALSE, ... = null }</code>
	 *
	 * @return
	 * @throws ContractFailure
	 */
	public boolean toBoolean() throws ContractFailure {
		Identifier trueId = new Identifier("true");
		Identifier falseId = new Identifier("false");
		CoreExprObject trueObj = nativeWrapper(nonNull(Boolean.TRUE), evaluator);
		CoreExprObject falseObj = nativeWrapper(nonNull(Boolean.FALSE), evaluator);
		MethodClosure trueCallback = new MethodClosure(Method.nullary(trueId, trueId), EMPTY_ENVIRONMENT.set(trueId, trueObj));
		MethodClosure falseCallback = new MethodClosure(Method.nullary(falseId, falseId), EMPTY_ENVIRONMENT.set(falseId, falseObj));
		CoreExprObject visitor = new CoreExprObject(NO_METHODS.set(trueId, trueCallback).set(falseId, falseCallback), evaluator);
		Object marker = call(new Identifier(Operator.MATCH.getMethodName()), visitor);
		if(marker instanceof Boolean)
			return (Boolean) marker;

		throw new ContractFailure("Not a boolean: "+this);
	}

	public @Nullable Object getNextObject() {
		return nextObject;
	}

	public static CoreExprObject wrap(Object x, CoreExprEvaluator evaluator) {
		return new CoreExprObject(NO_METHODS, x, evaluator);
	}
}
