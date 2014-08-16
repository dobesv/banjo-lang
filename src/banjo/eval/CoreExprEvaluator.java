package banjo.eval;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.core.Extend;
import banjo.dom.core.Inspect;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.Method;
import banjo.dom.core.MixfixFunctionIdentifier;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import fj.F;
import fj.data.List;
import fj.data.TreeMap;

public class CoreExprEvaluator {
	CoreExprObject emptyObject = new CoreExprObject(this);

	/**
	 * Call a method on an object using the rules of the evaluator.
	 *
	 * @param x Target object of the call
	 * @param methodName Method name to call
	 * @param argumentLists Arguments
	 * @return The result of the call
	 * @throws ContractFailure If a precondition or postcondition fails
	 */
	public Object call(Object x, Key methodName, List<List<Object>> argumentLists) {
		if(x instanceof CoreExprObject) {
			return ((CoreExprObject)x).call(methodName, argumentLists);
		}
		return CoreExprObject.wrap(x, this).call(methodName, argumentLists);
	}

	/**
	 * Extend any object with any other object, concatenating the methods of one
	 * to the methods of the other.
	 *
	 * @param base
	 * @param extension
	 * @return
	 */
	public Object extend(Object base, Object extension) {
		if(base instanceof CoreExprObject) {
			return ((CoreExprObject)base).extend(extension);
		} else {
			return CoreExprObject.wrap(base, this).extend(extension);
		}
	}

	public Object eval(CoreExpr expr, final TreeMap<Key,Object> environment) {
		final Object evalResult = nonNull(expr.acceptVisitor(new CoreExprVisitor<Object>() {
			@Override public Object badExpr(BadExpr expr) { return CoreExprEvaluator.this.emptyObject; }
			@Override public Object badIdentifier(BadIdentifier badIdentifier) { return CoreExprEvaluator.this.emptyObject; }

			@Override
			public Object call(final Call call) {
				List<List<Object>> argumentLists = call.getArgumentLists().map(new F<List<CoreExpr>, List<Object>>() {
					@Override
					public List<Object> f(@Nullable List<CoreExpr> a) {
						if(a == null) throw new NullPointerException();
						return a.map(new F<CoreExpr,Object>() {
							@Override
							public Object f(@Nullable CoreExpr arg) {
								if(arg == null) throw new NullPointerException();
								return eval(arg, environment);
							}
						});
					}
				});
				final Object actualTargetObject = eval(call.getObject(), environment);
				return CoreExprEvaluator.this.call(actualTargetObject, call.getName(), argumentLists);
			}

			@Override
			public Object objectLiteral(ObjectLiteral objectLiteral) {
				final TreeMap<Key, MethodClosure> methodMap = CoreExprObject.NO_METHODS;
				for(@NonNull @SuppressWarnings("null") final Method methodDef : objectLiteral.getMethods()) {
					final TreeMap<Key,Object> closure = environment;
					Key key = methodDef.getName();
					methodMap.set(key, new MethodClosure(methodDef, closure));
				}
				return new CoreExprObject(methodMap, CoreExprEvaluator.this);
			}

			public Object key(Key k) {
				@SuppressWarnings("null") @NonNull
				final Object result = environment.get(k).orSome(emptyObject);
				return result;
			}

			@Override
			public Object identifier(Identifier identifier) {
				return key(identifier);
			}

			@Override
			public Object stringLiteral(StringLiteral stringLiteral) {
				TreeMap<Key, MethodClosure> methods = CoreExprObject.NO_METHODS;
				// TODO Call factory method so we get the right methods added
				return new CoreExprObject(methods, stringLiteral.getString(), CoreExprEvaluator.this);
			}

			@Override
			public Object numberLiteral(NumberLiteral numberLiteral) {
				TreeMap<Key, MethodClosure> methods = CoreExprObject.NO_METHODS;
				// TODO Call factory method so we get the right methods added
				return new CoreExprObject(methods, numberLiteral.getNumber(), CoreExprEvaluator.this);
			}


//			public Object fromLong(Object value, final long val) {
//				final boolean negative = val < 0;
//				final long absVal = Math.abs(val);
//				for(int i=62; i >= 0; i--) {
//					// Find the highest set bit
//					if((absVal & (1 << i)) != 0) {
//						// Set the first bit
//						value = value.call("succ");
//						// Now push the remaining bits
//						for( ; i >= 0 ; i--) {
//							value = value.call("double"); // Shift left one
//							if((absVal & (1 << i)) != 0) {
//								value = value.call("succ"); // Add one to set the low bit if this bit was set
//							}
//						}
//						if(negative)
//							value = value.call("-");
//						break;
//					}
//				}
//				return value;
//			}
//			public Object fromInt(Object value, final int val) {
//				final boolean negative = val < 0;
//				final long absVal = Math.abs(val);
//				for(int i=30; i >= 0; i--) {
//					// Find the highest set bit
//					if((absVal & (1 << i)) != 0) {
//						// Set the first bit
//						value = ExprEvaluator.this.call(value, "succ");
//						// Now push the remaining bits
//						for( ; i >= 0 ; i--) {
//							value = value.call("double"); // Shift left one
//							if((absVal & (1 << i)) != 0) {
//								value = value.call("succ"); // Add one to set the low bit if this bit was set
//							}
//						}
//						if(negative)
//							value = ExprEvaluator.this.call(value, "-");
//						break;
//					}
//				}
//				return value;
//			}

			@Override
			public Object operator(OperatorRef operatorRef) {
				throw new Error("Not implemented!");
			}

			@Override
			public Object extend(Extend extend) {
				final Object base = eval(extend.getBase(), environment);
				final Object extension = eval(extend.getExtension(), environment);
				return CoreExprEvaluator.this.extend(base, extension);
			}

			@Override
			public Object inspect(Inspect inspect) {
				throw new Error("Not implemented!");
			}

			@Override
			public Object listLiteral(ListLiteral listLiteral) {
				throw new Error("Not implemented!");
			}
			@Override
			public Object method(Method method) {
				throw new Error("Not implemented!");
			}
			@Override
			public Object mixfixFunctionIdentifier(MixfixFunctionIdentifier identifier) {
				return key(identifier);
			}
			@Override
			public Object anonymous() {
				return key(Key.ANONYMOUS);
			}
		}));
		return evalResult;
	}

}
