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
	EvalObject emptyObject = new CoreExprObject(this);

	public EvalObject eval(CoreExpr expr, final TreeMap<Key,EvalObject> environment) {
		final CoreExprEvaluator evaluator = this;
		final EvalObject evalResult = nonNull(expr.acceptVisitor(new CoreExprVisitor<EvalObject>() {
			@Override public EvalObject badExpr(BadExpr expr) { return evaluator.emptyObject; }
			@Override public EvalObject badIdentifier(BadIdentifier badIdentifier) { return evaluator.emptyObject; }

			@Override
			public EvalObject call(final Call call) {
				// This is where the laziness is supposed to come in ...
				List<List<EvalObject>> argumentLists = call.getArgumentLists().map(new F<List<CoreExpr>, List<EvalObject>>() {
					@Override
					public List<EvalObject> f(@Nullable List<CoreExpr> a) {
						if(a == null) throw new NullPointerException();
						return a.map(new F<CoreExpr,EvalObject>() {
							@Override
							public EvalObject f(@Nullable CoreExpr arg) {
								if(arg == null) throw new NullPointerException();
								return eval(arg, environment);
							}
						});
					}
				});
				final EvalObject target = eval(call.getObject(), environment);

				// Lazy by default
				return new DeferredCall(target, call.getName(), argumentLists, call.isOptional(), target, call.isCallNext());
			}

			@Override
			public EvalObject objectLiteral(ObjectLiteral objectLiteral) {
				TreeMap<Key, List<MethodClosure>> methodMap = CoreExprObject.NO_METHODS;
				for(@NonNull @SuppressWarnings("null") final Method methodDef : objectLiteral.getMethods()) {
					methodMap = methodMap.set(methodDef.getName(), methodMap.get(methodDef.getName()).orSome(List.<MethodClosure>nil()).cons(new MethodClosure(methodDef, environment)));
				}
				return new CoreExprObject(methodMap, evaluator);
			}

			public EvalObject key(Key k) {
				@SuppressWarnings("null") @NonNull
				final EvalObject result = environment.get(k).orSome(emptyObject);
				return result;
			}

			@Override
			public EvalObject identifier(Identifier identifier) {
				return key(identifier);
			}

			@Override
			public EvalObject stringLiteral(StringLiteral stringLiteral) {
				// TODO Call factory method so we get the right methods added
				return new CoreExprObject(CoreExprObject.NO_METHODS, evaluator);
			}

			@Override
			public EvalObject numberLiteral(NumberLiteral numberLiteral) {
				// TODO Call factory method so we get the right methods added
				return new CoreExprObject(CoreExprObject.NO_METHODS, evaluator);
			}

			@Override
			public EvalObject operator(OperatorRef operatorRef) {
				throw new Error("Not implemented!");
			}

			@Override
			public EvalObject extend(Extend extend) {
				final EvalObject base = eval(extend.getBase(), environment);
				final EvalObject extension = eval(extend.getExtension(), environment);
				return base.extend(extension);
			}

			@Override
			public EvalObject inspect(Inspect inspect) {
				throw new Error("Not implemented!");
			}

			@Override
			public EvalObject listLiteral(ListLiteral listLiteral) {
				throw new Error("Not implemented!");
			}
			@Override
			public EvalObject method(Method method) {
				throw new Error("Not implemented!");
			}
			@Override
			public EvalObject mixfixFunctionIdentifier(MixfixFunctionIdentifier identifier) {
				return key(identifier);
			}
			@Override
			public EvalObject anonymous() {
				return key(Key.ANONYMOUS);
			}
		}));
		return evalResult;
	}

}
