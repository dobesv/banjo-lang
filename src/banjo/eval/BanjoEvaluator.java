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

public class BanjoEvaluator {
	BanjoObject emptyObject = new BanjoObject(this);
	public BanjoObject eval(CoreExpr expr, final TreeMap<String,BanjoObject> environment) {
		final BanjoObject evalResult = nonNull(expr.acceptVisitor(new CoreExprVisitor<BanjoObject>() {
			@Override public BanjoObject badExpr(SourceFileRange range, String messageTemplate, Object... args) { return BanjoEvaluator.this.emptyObject; }
			@Override public BanjoObject badIdentifier(BadIdentifier badIdentifier) { return BanjoEvaluator.this.emptyObject; }

			@Override
			public BanjoObject call(final Call call) {
				final List<BanjoMessage.MessagePart> messageParts = call.getParts().map(new F<Call.MessagePart, BanjoMessage.MessagePart>() {
					@Override
					public MessagePart f(@Nullable Call.MessagePart a) {
						if(a == null) throw new NullPointerException();
						final List<BanjoObject> actualArgs = a.getArguments().map(new F<CoreExpr,BanjoObject>() {
							@Override
							public BanjoObject f(@Nullable CoreExpr arg) {
								return eval(nonNull(arg), environment);
							}
						});
						return new BanjoMessage.MessagePart(a.getKey().getKeyString(), actualArgs);
					}
				});
				BanjoMessage message = new BanjoMessage(messageParts);
				final BanjoObject actualTargetObject = eval(call.getObject(), environment);
				return actualTargetObject.call(message);
			}

			@Override
			public BanjoObject objectLiteral(ObjectLiteral objectLiteral) {
				final TreeMap<List<String>, MethodClosure> methodMap = BanjoObject.NO_METHODS;
				for(@NonNull @SuppressWarnings("null") final Method methodDef : objectLiteral.getMethods()) {
					final DefRefAnalyser defRefAnalyser = new DefRefAnalyser();
					final Analysis analysis = defRefAnalyser.analyseMethod(methodDef);
					final TreeMap<String,BanjoObject> closure = BanjoObject.EMPTY_ENVIRONMENT;
					for(final Key freeRef : analysis.getFree()) {
						final String id = freeRef.getKeyString();
						closure.set(id, environment.get(id).orSome(BanjoEvaluator.this.emptyObject));
					}
					List<String> key = methodDef.getParts().map(new F<SignaturePart,String>() { 
						public String f(@Nullable SignaturePart a) {
							if(a == null) throw new NullPointerException();
							return a.getKey().getKeyString(); 
						}
					});
					methodMap.set(key, new MethodClosure(methodDef, closure));
				}
				return new BanjoObject(methodMap, BanjoEvaluator.this);
			}

			@Override
			public BanjoObject identifier(Identifier identifier) {
				@SuppressWarnings("null") @NonNull
				final BanjoObject result = environment.get(identifier.getId()).orSome(BanjoEvaluator.this.emptyObject);
				return result;
			}

			@Override
			public BanjoObject stringLiteral(StringLiteral stringLiteral) {
				// Generate strings by starting with an empty string and append characters by number
				BanjoObject value = identifier(Identifier.EMPTY_STRING);
				final BanjoObject zero = identifier(Identifier.ZERO);
				final String s = stringLiteral.getString();
				for(int i=0, count = s.length(); i < count; ) {
					final int cp = s.codePointAt(i);
					value = value.call("appendCodePoint", fromInt(zero, cp));
					i += Character.charCount(cp);
				}
				return value;
			}

			@Override
			public BanjoObject numberLiteral(NumberLiteral numberLiteral) {
				// TODO Perhaps we can require there to be special identifiers "1" and "0" in the environment that we use to construct numbers?  Or even "0".."9" ?  Or "0" .. "10" ?
				final Number number = numberLiteral.getNumber();
				final BanjoObject zero = identifier(Identifier.ZERO);
				if(number instanceof Long) {
					return fromLong(zero, number.longValue());
				} else if(number instanceof Integer) {
					return fromInt(zero, number.intValue());
				}
				return BanjoEvaluator.this.emptyObject;
			}


			public BanjoObject fromLong(BanjoObject value, final long val) {
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
			public BanjoObject fromInt(BanjoObject value, final int val) {
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
			public BanjoObject operator(OperatorRef operatorRef) {
				throw new Error("Not implemented!");
			}

			@Override
			public BanjoObject extend(Extend extend) {
				final BanjoObject base = eval(extend.getBase(), environment);
				final BanjoObject extension = eval(extend.getExtension(), environment);
				return base.extend(extension);
			}

			@Override
			@Nullable
			public BanjoObject inspect(Inspect inspect) {
				throw new Error("Not implemented!");
			}

			@Override
			@Nullable
			public BanjoObject listLiteral(ListLiteral listLiteral) {
				throw new Error("Not implemented!");
			}
		}));
		return evalResult;
	}

}
