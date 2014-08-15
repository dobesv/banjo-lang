package banjo.eval;

import static banjo.parser.util.Check.nonNull;

import java.util.Arrays;
import java.util.Iterator;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.CoreExpr;
import banjo.dom.core.Method;
import banjo.dom.core.Method.SignaturePart;
import banjo.dom.core.MethodFormalArgument;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.source.Operator;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.eval.BanjoMessage.MessagePart;
import banjo.parser.util.SourceFileRange;
import fj.F;
import fj.F2;
import fj.Ord;
import fj.P2;
import fj.data.List;
import fj.data.TreeMap;
import static fj.data.List.list;

/**
 * An object that carries method definitions plus a captured environment.
 */
public final class EvalObject {
	@NonNull @SuppressWarnings("null")
	public static final TreeMap<Key, MethodClosure> NO_METHODS = TreeMap.empty(Ord.listOrd(Ord.stringOrd));

	@NonNull @SuppressWarnings("null")
	public static final TreeMap<String,EvalObject> EMPTY_ENVIRONMENT = TreeMap.empty(Ord.stringOrd);

	private final ExprEvaluator evaluator;
	private final TreeMap<Key, MethodClosure> methods;
	private final @Nullable Object javaObject;

	public EvalObject(ExprEvaluator evaluator) {
		this(NO_METHODS, evaluator);
	}

	public EvalObject(TreeMap<Key, MethodClosure> methods, ExprEvaluator evaluator) {
		this(methods, null, evaluator);
	}

	static public EvalObject nativeWrapper(Object obj, ExprEvaluator evaluator) {
		return new EvalObject(NO_METHODS, obj, evaluator);
	}

	public EvalObject(TreeMap<Key, MethodClosure> methods, @Nullable Object javaObject, ExprEvaluator evaluator) {
		super();
		this.methods = methods;
		this.evaluator = evaluator;
		this.javaObject = javaObject;
	}

	public TreeMap<Key, MethodClosure> getMethods() {
		return this.methods;
	}

	public EvalObject extend(EvalObject extension) {
		final TreeMap<Key, MethodClosure> newMethods = this.methods;
		for(final P2<Key,MethodClosure> p : extension.getMethods()) {
			newMethods.set(p._1(), p._2());
		}
		return new EvalObject(newMethods, this.evaluator);
	}

	public EvalObject call(BanjoMessage message) {
		Key key = message.getParts().map(new F<BanjoMessage.MessagePart,String>() {
			@Override
			public String f(@Nullable MessagePart a) {
				if(a == null) throw new NullPointerException();
				return a.getKey();
			}
		});

		final MethodClosure impl = this.methods.get(key).toNull();
		if(impl == null)
			return this.evaluator.emptyObject;
		final Method methodDef = impl.getMethod();
		@NonNull
		TreeMap<String, EvalObject> newEnvironment = impl.getEnvironment();

		// Add arguments to the environment
		if(methodDef.hasSelfArg())
			newEnvironment = newEnvironment.set(methodDef.getSelfName().getKeyString(), this);

		newEnvironment = nonNull(message.getParts().zip(methodDef.getParts()).foldRight(new F2<P2<BanjoMessage.MessagePart, Method.SignaturePart>, TreeMap<String, EvalObject>, TreeMap<String, EvalObject>>() {
			public @Nullable fj.data.TreeMap<String,EvalObject> f(@Nullable fj.P2<MessagePart,banjo.dom.core.Method.SignaturePart> a, @Nullable TreeMap<String,EvalObject> b) {
				if(a == null) throw new NullPointerException();
				if(b == null) throw new NullPointerException();
				MessagePart messagePart = a._1();
				SignaturePart signaturePart = a._2();
				if(messagePart == null) throw new NullPointerException();
				if(signaturePart == null) throw new NullPointerException();

				return messagePart.getArguments().zip(signaturePart.getArguments()).foldRight(new F2<P2<EvalObject,MethodFormalArgument>,TreeMap<String,EvalObject>, TreeMap<String, EvalObject>> () {
					@Override
					public TreeMap<String, EvalObject> f(
							@Nullable P2<EvalObject, MethodFormalArgument> aa,
							@Nullable TreeMap<String, EvalObject> bb) {
						if(aa == null) throw new NullPointerException();
						if(bb == null) throw new NullPointerException();
						MethodFormalArgument argFormal = aa._2();
						if(argFormal.hasAssertion())
							throw new Error("TODO: Assertions not implemented");
						EvalObject argActual = aa._1();
						return bb.set(argFormal.getName().getKeyString(), argActual);
					}
				}, b);
			}

		}, newEnvironment));

		if(methodDef.hasPostcondition())
			throw new Error("TODO: Guarantee not implemented");

		// Evaluate the method body
		return this.evaluator.eval(methodDef.getBody(), newEnvironment);

	}

	public EvalObject call(Key methodName, EvalObject ... arguments) {
		return call(new BanjoMessage(List.single(new BanjoMessage.MessagePart(methodName, List.<EvalObject>list(arguments)))));
	}

	public boolean hasMethod(Key name) {
		return hasMethod(list(name));
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
	 */
	public @Nullable Boolean toBoolean(ExprEvaluator evaluator) {
		if(javaObject instanceof Boolean)
			return (Boolean) javaObject;
		Identifier trueId = new Identifier(SourceFileRange.SYNTHETIC, "true");
		Identifier falseId = new Identifier(SourceFileRange.SYNTHETIC, "false");
		EvalObject trueObj = nativeWrapper(nonNull(Boolean.TRUE), evaluator);
		EvalObject falseObj = nativeWrapper(nonNull(Boolean.FALSE), evaluator);
		MethodClosure trueCallback = new MethodClosure(Method.nullary(trueId, trueId), EMPTY_ENVIRONMENT.set("true", trueObj));
		MethodClosure falseCallback = new MethodClosure(Method.nullary(falseId, falseId), EMPTY_ENVIRONMENT.set("false", falseObj));
		EvalObject visitor = new EvalObject(NO_METHODS.set(list("true"), trueCallback).set(list("false"), falseCallback), evaluator);
		EvalObject marker = call(Operator.MATCH.getMethodName(), visitor);
		if(marker.getJavaObject() instanceof Boolean)
			return (Boolean) marker.getJavaObject();
		return null;
	}

	public @Nullable Object getJavaObject() {
		return javaObject;
	}
}
