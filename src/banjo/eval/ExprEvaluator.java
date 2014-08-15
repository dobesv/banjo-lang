package banjo.eval;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.analysis.DefRefAnalyser;
import banjo.analysis.DefRefAnalyser.Analysis;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.core.Extend;
import banjo.dom.core.Inspect;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.Method;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.core.Method.SignaturePart;
import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.eval.BanjoMessage.MessagePart;
import banjo.parser.util.SourceFileRange;
import fj.F;
import fj.data.List;
import fj.data.TreeMap;

public class ExprEvaluator {
	EvalObject emptyObject = new EvalObject(this);
	public EvalObject eval(CoreExpr expr, final TreeMap<String,EvalObject> environment) {
		final EvalObject evalResult = nonNull(expr.acceptVisitor(new CoreExprVisitor<EvalObject>() {
			@Override public EvalObject badExpr(SourceFileRange range, String messageTemplate, Object... args) { return ExprEvaluator.this.emptyObject; }
			@Override public EvalObject badIdentifier(BadIdentifier badIdentifier) { return ExprEvaluator.this.emptyObject; }

			@Override
			public EvalObject call(final Call call) {
				final List<BanjoMessage.MessagePart> messageParts = call.getParts().map(new F<Call.MessagePart, BanjoMessage.MessagePart>() {
					@Override
					public MessagePart f(@Nullable Call.MessagePart a) {
						if(a == null) throw new NullPointerException();
						final List<EvalObject> actualArgs = a.getArguments().map(new F<CoreExpr,EvalObject>() {
							@Override
							public EvalObject f(@Nullable CoreExpr arg) {
								return eval(nonNull(arg), environment);
							}
						});
						return new BanjoMessage.MessagePart(a.getKey().getKeyString(), actualArgs);
					}
				});
				BanjoMessage message = new BanjoMessage(messageParts);
				final EvalObject actualTargetObject = eval(call.getObject(), environment);
				return actualTargetObject.call(message);
			}

			@Override
			public EvalObject objectLiteral(ObjectLiteral objectLiteral) {
				final TreeMap<Key, MethodClosure> methodMap = EvalObject.NO_METHODS;
				for(@NonNull @SuppressWarnings("null") final Method methodDef : objectLiteral.getMethods()) {
					final TreeMap<String,EvalObject> closure = environment;
					Key key = methodDef.getName();
					methodMap.set(key, new MethodClosure(methodDef, closure));
				}
				return new EvalObject(methodMap, ExprEvaluator.this);
			}

			@Override
			public EvalObject identifier(Identifier identifier) {
				@SuppressWarnings("null") @NonNull
				final EvalObject result = environment.get(identifier.getId()).orSome(ExprEvaluator.this.emptyObject);
				return result;
			}

			@Override
			public EvalObject stringLiteral(StringLiteral stringLiteral) {
				// Generate strings by starting with an empty string and append characters by number
				EvalObject value = identifier(Identifier.EMPTY_STRING);
				final EvalObject zero = identifier(Identifier.ZERO);
				final String s = stringLiteral.getString();
				for(int i=0, count = s.length(); i < count; ) {
					final int cp = s.codePointAt(i);
					value = value.call("appendCodePoint", fromInt(zero, cp));
					i += Character.charCount(cp);
				}
				return value;
			}

			@Override
			public EvalObject numberLiteral(NumberLiteral numberLiteral) {
				// TODO Perhaps we can require there to be special identifiers "1" and "0" in the environment that we use to construct numbers?  Or even "0".."9" ?  Or "0" .. "10" ?
				final Number number = numberLiteral.getNumber();
				final EvalObject zero = identifier(Identifier.ZERO);
				if(number instanceof Long) {
					return fromLong(zero, number.longValue());
				} else if(number instanceof Integer) {
					return fromInt(zero, number.intValue());
				}
				return ExprEvaluator.this.emptyObject;
			}


			public EvalObject fromLong(EvalObject value, final long val) {
				final boolean negative = val < 0;
				final long absVal = Math.abs(val);
				for(int i=62; i >= 0; i--) {
					// Find the highest set bit
					if((absVal & (1 << i)) != 0) {
						// Set the first bit
						value = value.call("succ");
						// Now push the remaining bits
						for( ; i >= 0 ; i--) {
							value = value.call("double"); // Shift left one
							if((absVal & (1 << i)) != 0) {
								value = value.call("succ"); // Add one to set the low bit if this bit was set
							}
						}
						if(negative)
							value = value.call("-");
						break;
					}
				}
				return value;
			}
			public EvalObject fromInt(EvalObject value, final int val) {
				final boolean negative = val < 0;
				final long absVal = Math.abs(val);
				for(int i=30; i >= 0; i--) {
					// Find the highest set bit
					if((absVal & (1 << i)) != 0) {
						// Set the first bit
						value = value.call("succ");
						// Now push the remaining bits
						for( ; i >= 0 ; i--) {
							value = value.call("double"); // Shift left one
							if((absVal & (1 << i)) != 0) {
								value = value.call("succ"); // Add one to set the low bit if this bit was set
							}
						}
						if(negative)
							value = value.call("-");
						break;
					}
				}
				return value;
			}

			@Override
			@Nullable
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
			@Nullable
			public EvalObject inspect(Inspect inspect) {
				throw new Error("Not implemented!");
			}

			@Override
			@Nullable
			public EvalObject listLiteral(ListLiteral listLiteral) {
				throw new Error("Not implemented!");
			}
		}));
		return evalResult;
	}

}
