package banjo.eval.coreexpr;

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
import fj.F2;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.TreeMap;

public class EvalEnvironment implements CoreExprVisitor<EvalResult> {
	@Nullable
	public final EvalEnvironment parent;
	public final TreeMap<Key, CoreExpr> rootBindings;
	public final TreeMap<Key, EvalResult> bindings;

	public static final TreeMap<Key, EvalResult> EMPTY_BINDINGS = TreeMap
			.empty(Key.ORD);

	EvalEnvironment(@Nullable EvalEnvironment parent,
			TreeMap<Key, CoreExpr> rootBindings,
			TreeMap<Key, EvalResult> bindings) {
		super();
		this.parent = parent;
		this.rootBindings = rootBindings;
		this.bindings = bindings;
	}

	public EvalEnvironment(EvalEnvironment parent,
			TreeMap<Key, EvalResult> bindings) {
		this(parent, parent.rootBindings, bindings);
	}

	public EvalEnvironment(TreeMap<Key, CoreExpr> rootBindings) {
		this(null, rootBindings, EMPTY_BINDINGS);
	}

	public EvalResult failure(String variant, String info) {
		return failure(variant, stringLiteral(new StringLiteral(info)));
	}

	public EvalResult failure(String variant, EvalResult info) {
		final EvalEnvironment rootEnv = getRootEnvironment();
		TreeMap<Key, EvalResult> newBindings = rootEnv.bindings.set(
				new Identifier("failure info"), info);

		return new ObjectLiteral(Method.nullary(new Identifier("is failure"),
				new Identifier("true"))).acceptVisitor(new EvalEnvironment(
				rootEnv, newBindings));
	}

	public EvalEnvironment getRootEnvironment() {
		final EvalEnvironment parent = this.parent;
		return parent == null ? this : parent.getRootEnvironment();
	}

	@Override
	public EvalResult badExpr(BadExpr badExpr) {
		// Non-reducible
		String message = badExpr.getMessage();
		return badExpr(message);
	}

	private EvalResult badExpr(String message) {
		return failure("bad expression", message);
	}

	@Override
	public EvalResult anonymous() {
		return badExpr("ANONYMOUS not expected to be evaluated");
	}

	@Override
	public EvalResult badIdentifier(BadIdentifier badIdentifier) {
		return badExpr(badIdentifier.getMessage());
	}

	@Override
	public EvalResult call(Call call) {
		EvalResult object = call.getObject().acceptVisitor(this);

		if (call.isCallNext()) {
			// Tricky stuff - have to pass in a self arg that will
			// result in a call next calling the right next method
			// only within this method.
			throw new Error("TODO");
		}

		@Nullable
		EvalResult methodBinding = object.findMethod(call.getName(),
				call.isCallNext());
		if (methodBinding == null || methodBinding.currentMethod == null) {
			if (call.isOptional()) {
				return new ListLiteral(List.nil()).acceptVisitor(this);
			} else {
				return failure("missing method definition", call.getName()
						.toSource());
			}
		}
		Method method = nonNull(methodBinding.currentMethod);
		EvalEnvironment closure = methodBinding.environment;
		TreeMap<Key, EvalResult> newBindings = method
				.getArgumentLists().zip(call.getArgumentLists()).foldLeft(
				(bindings1, pair1) -> pair1
						._1()
						.zip(pair1._2())
						.foldLeft(
								(bindings2, pair2) -> bindings2.set(
										pair2._1(),
										pair2._2().acceptVisitor(
												EvalEnvironment.this)),
								bindings1), closure.bindings.set(
						method.getSelfArg(), methodBinding));

		EvalEnvironment newEnvironment = new EvalEnvironment(this, newBindings);
		// TODO Precondition, postcondition
		final EvalResult result = method.getBody()
				.acceptVisitor(newEnvironment);
		if (call.isOptional()) {
			// TODO Be more specific about the failure - maybe some will still
			// propagate through ?
			if (result.isFailure()) {
				return new ListLiteral(List.nil()).acceptVisitor(this);
			} else {
				result.useIn((tempId, tempEnv) -> new ListLiteral(List
						.single(tempId)).acceptVisitor(tempEnv));
			}
		}
		return result;
	}

	@Override
	public EvalResult extend(Extend extend) {
		EvalResult base = extend.getBase().acceptVisitor(this);
		EvalResult ext = extend.getExtension().acceptVisitor(this);
		return new EvalResult(base, ext.object, ext.environment);
	}

	@Override
	public EvalResult identifier(Identifier id) {
		return this.bindings.get(id)
			.orElse(P.lazy(u -> this.rootBindings.get(id).map(lazyValue -> lazyValue.acceptVisitor(getRootEnvironment()))))
			.orSome(P.lazy(u -> failure("unbound identifier", id.toString())));
	}

	@Override
	public EvalResult stringLiteral(StringLiteral stringLiteral) {
		return stringLiteral.toConstructionExpression().acceptVisitor(
				getRootEnvironment());
	}

	@Override
	public EvalResult inspect(Inspect inspect) {
		throw new Error("TODO");
	}

	@Override
	public EvalResult listLiteral(ListLiteral listLiteral) {
		return listLiteral.toConstructionExpression().acceptVisitor(
				getRootEnvironment());
	}

	public EvalResult mixfixFunctionIdentifier(MixfixFunctionIdentifier id) {
		return this.bindings.get(id).orSome(P.lazy(u -> failure("unbound identifier", id.toString())));
	}

	@Override
	public EvalResult numberLiteral(NumberLiteral numberLiteral) {
		CoreExpr ctor = numberLiteral.toConstructionExpression();
		return ctor.acceptVisitor(getRootEnvironment());
	}

	@Override
	public EvalResult objectLiteral(ObjectLiteral objectLiteral) {
		return new EvalResult(objectLiteral, this);
	}

	@Override
	public EvalResult operator(OperatorRef id) {
		throw new Error("Should not happen");
	}
}
