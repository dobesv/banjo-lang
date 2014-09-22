package banjo.eval;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.ListLiteral;
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
public final class CoreExprObject implements EvalObject {
	@NonNull @SuppressWarnings("null")
	public static final TreeMap<Key, List<MethodClosure>> NO_METHODS = TreeMap.empty(Key.ORD);

	@NonNull @SuppressWarnings("null")
	public static final TreeMap<Key,EvalObject> EMPTY_ENVIRONMENT = TreeMap.empty(Key.ORD);

	private final CoreExprEvaluator evaluator;
	private final TreeMap<Key, List<MethodClosure>> methods;

	public CoreExprObject(CoreExprEvaluator evaluator) {
		this(NO_METHODS, evaluator);
	}

	static public CoreExprObject nativeWrapper(Object obj, CoreExprEvaluator evaluator) {
		return new CoreExprObject(NO_METHODS, evaluator);
	}

	public CoreExprObject(TreeMap<Key, List<MethodClosure>> methods, CoreExprEvaluator evaluator) {
		super();
		this.methods = methods;
		this.evaluator = evaluator;
	}

	public TreeMap<Key, List<MethodClosure>> getMethods() {
		return this.methods;
	}

	public EvalObject extend(EvalObject ext) {
		if(ext instanceof CoreExprObject) {
			CoreExprObject extension = (CoreExprObject)ext;
			TreeMap<Key, List<MethodClosure>> newMethods = this.methods;
			for(final P2<Key,List<MethodClosure>> p : extension.getMethods()) {
				newMethods = newMethods.set(p._1(), p._2().append(newMethods.get(p._1()).orSome(List.<MethodClosure>nil())));
			}
			return new CoreExprObject(newMethods, this.evaluator);
		} else {
			return new ExtendedObject(this, ext);
		}
	}

	class NextMethodCaller implements EvalObject {
		private final Key methodName;
		private final List<MethodClosure> nextImpls;
		private final EvalObject selfArg;

		public NextMethodCaller(Key methodName, List<MethodClosure> impls, EvalObject selfArg) {
			super();
			this.methodName = methodName;
			this.nextImpls = impls;
			this.selfArg = selfArg;
		}

		@Override
		public EvalObject call(Key methodName,
				List<List<EvalObject>> argumentLists, boolean optional,
				EvalObject selfArg, boolean callNext) {
			if(callNext && methodName.equals(this.methodName)) {
				return callMethod(methodName, argumentLists, optional, this.selfArg, nextImpls);
			}
			return CoreExprObject.this.call(methodName, argumentLists, optional, selfArg, callNext);
		}

		@Override
		public EvalObject extend(EvalObject extension) {
			return CoreExprObject.this.extend(extension);
		}
	}

	public EvalObject call(Key methodName, List<List<EvalObject>> argumentLists, boolean optional, EvalObject selfArg, boolean callNext) {
		if(callNext) {
			return EvalUtil.noNextMethod(methodName, optional);
		}
		final List<MethodClosure> impls = nonNull(this.methods.get(methodName).orSome(List.<MethodClosure>nil()));
		return callMethod(methodName, argumentLists, optional, selfArg, impls);
	}

	private EvalObject callMethod(Key methodName,
			List<List<EvalObject>> argumentLists, boolean optional,
			EvalObject selfArg, final List<MethodClosure> impls) throws ContractFailure, Error {
		if(impls.isEmpty()) {
			if(optional) {
				// Return empty list
				// TODO There will be a better way to do this ...
				return evaluator.eval(ListLiteral.EMPTY_LIST, EMPTY_ENVIRONMENT);
			} else {
				// If we have no implementation, delegate to the "next"
				return selfArg.call(methodName, argumentLists, optional, selfArg, true);
			}
		}
		MethodClosure impl = impls.head();

		final Method methodDef = impl.getMethod();
		@NonNull
		TreeMap<Key, EvalObject> newEnvironment = impl.getEnvironment();


		// Add arguments to the environment
		if(methodDef.hasSelfArg()) {
			// If we have more implementations, queue them up into selfArg
			EvalObject nextSelfArg = impls.tail().isEmpty() ? selfArg : new NextMethodCaller(methodName, impls.tail(), selfArg);
			newEnvironment = newEnvironment.set(methodDef.getSelfArg(), nextSelfArg);
		}

		newEnvironment = nonNull(methodDef.getArgumentLists().zip(argumentLists).foldRight(new F2<P2<List<Key>, List<EvalObject>>, TreeMap<Key, EvalObject>, TreeMap<Key, EvalObject>>() {
			public @Nullable TreeMap<Key,EvalObject> f(@Nullable P2<List<Key>, List<EvalObject>> a, @Nullable TreeMap<Key, EvalObject> b) {
				if(a == null) throw new NullPointerException();
				if(b == null) throw new NullPointerException();
				List<EvalObject> argValues = nonNull(a._2());
				List<Key> argNames = nonNull(a._1());
				return argNames.zip(argValues).foldRight(new F2<P2<Key,EvalObject>,TreeMap<Key,EvalObject>, TreeMap<Key, EvalObject>> () {
					@Override
					public TreeMap<Key, EvalObject> f(@Nullable P2<Key, EvalObject> aa, @Nullable TreeMap<Key,EvalObject> bb) {
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
				if(optional)
				{

				}
				throw new ContractFailure("Precondition failed: "+methodDef.getPrecondition());
			}
		}
		if(methodDef.hasPostcondition())
			throw new Error("TODO: Postcondition not implemented");

		// Evaluate the method body
		return this.evaluator.eval(methodDef.getBody(), newEnvironment);
	}

	public Object call(Key methodName, Object ... arguments) throws ContractFailure {
		return call(methodName, List.single(List.<Object>list(arguments)), false);
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
	public boolean toBoolean() {
		Identifier trueId = new Identifier("true");
		Identifier falseId = new Identifier("false");
		CoreExprObject trueObj = nativeWrapper(nonNull(Boolean.TRUE), evaluator);
		CoreExprObject falseObj = nativeWrapper(nonNull(Boolean.FALSE), evaluator);
		MethodClosure trueCallback = new MethodClosure(Method.nullary(trueId, trueId), EMPTY_ENVIRONMENT.set(trueId, trueObj));
		MethodClosure falseCallback = new MethodClosure(Method.nullary(falseId, falseId), EMPTY_ENVIRONMENT.set(falseId, falseObj));
		CoreExprObject visitor = new CoreExprObject(NO_METHODS.set(trueId, List.single(trueCallback)).set(falseId, List.single(falseCallback)), evaluator);
		Object marker = call(new Identifier(Operator.MATCH.getMethodName()), visitor);
		if(marker instanceof Boolean)
			return (Boolean) marker;

		throw new ContractFailure("Not a boolean: "+this);
	}

	public static CoreExprObject wrap(Object x, CoreExprEvaluator evaluator) {
		return new CoreExprObject(NO_METHODS, evaluator);
	}
}
