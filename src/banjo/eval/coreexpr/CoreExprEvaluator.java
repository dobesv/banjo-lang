package banjo.eval.coreexpr;

import static java.util.Objects.requireNonNull;
import banjo.dom.core.BadCoreExpr;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.core.Extend;
import banjo.dom.core.FreeVariableGatherer;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.Inspect;
import banjo.dom.core.Let;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.core.Slot;
import banjo.dom.core.SlotReference;
import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.StringLiteral;
import banjo.eval.EvalUtil;
import banjo.eval.ProjectLoader;
import banjo.eval.Value;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public class CoreExprEvaluator implements CoreExprVisitor<Object> {
	public final CoreExprEvaluator parent;
	public final TreeMap<Identifier, Binding> bindings;

	public static final TreeMap<Identifier, Binding> EMPTY_BINDINGS = TreeMap.empty(Identifier.ORD);

	private final FreeVariableGatherer freeVarGatherer;

	private CoreExprEvaluator(CoreExprEvaluator parent, List<P2<Identifier, Binding>> bindings, FreeVariableGatherer freeVarGatherer) {
		super();
		this.parent = parent;
		bindings.forEach(this::bindUnboundThunk);
		this.bindings = EMPTY_BINDINGS.union(requireNonNull(bindings));
		this.freeVarGatherer = requireNonNull(freeVarGatherer);
	}

	public void bindUnboundThunk(P2<Identifier,Binding> p) {
		Object value = p._2().value;
		if(value instanceof LazyValue) {
			LazyValue thunk = (LazyValue)value;
			if(thunk.evaluator == null)
				thunk.evaluator = this;
		}
	}

	public Object lazy(CoreExpr e) {
		return new LazyValue(e, this);
	}

	public Object failure(String variant, String info) {
		return failure(variant, new StringLiteral(info));
	}

	public Object failure(String variant, CoreExpr info) {
		return badExpr(new BadCoreExpr(SourceFileRange.EMPTY_LIST, "%s: %s", variant, info.toSource()));
	}

	public CoreExprEvaluator getRootEnvironment() {
		final CoreExprEvaluator parent = this.parent;
		return parent == null ? this : parent.getRootEnvironment();
	}

	@Override
	public Object badExpr(BadCoreExpr badExpr) {
		return new IllegalStateException(badExpr.getMessage());
	}

	private Object badExpr(String message) {
		return failure("bad expression", message);
	}

	@Override
	public Object badIdentifier(BadIdentifier badIdentifier) {
		return badExpr(badIdentifier.getMessage());
	}

	@Override
	public Object call(Call call) {
		final List<Object> argsList = call.args.map(this::lazy);
		if(call.target instanceof SlotReference) {
			SlotReference slotRef = (SlotReference) call.target;
			final Object target = evaluate(slotRef.object);
			return EvalUtil.callMethod(target, slotRef.slotName.id, null, null, argsList);
		} else {
			final Object target = evaluate(call.target);
			return EvalUtil.call(target, target, null, argsList);
		}
	}

	@Override
	public Object extend(Extend extend) {
		return new ExtendedObject(lazy(extend.base), lazy(extend.extension));
	}

	@Override
	public Object identifier(Identifier id) {
		return getBinding(id)
				.map(b -> b.value)
				.orSome(P.lazy(u -> unboundIdentifier(id)));
    }

	protected Object unboundIdentifier(Identifier id) {
	    return FunctionInstance.addTrace(new IllegalStateException(String.format("Unknown variable '%s'", id.id)), id);
    }

	protected Option<Binding> getBinding(Identifier id) {
		final Option<Binding> binding = this.bindings.get(id);
		if(parent != null && binding.isNone()) return parent.getBinding(id);
		else return binding;
    }

	@Override
	public Object stringLiteral(StringLiteral stringLiteral) {
		final Object stringWrapFn = getRootEnvironment().identifier(new Identifier("java string"));
		return EvalUtil.call(stringWrapFn, List.single(stringLiteral.string));
	}

	@Override
	public Object inspect(Inspect inspect) {
		throw new Error("TODO");
	}

	@Override
	public Object listLiteral(ListLiteral listLiteral) {
		final Object listWrapFn = getRootEnvironment().identifier(new Identifier("java list"));
		return EvalUtil.call(listWrapFn, List.single(listLiteral.elements.map(this::lazy)));
	}

	@Override
	public Object numberLiteral(NumberLiteral numberLiteral) {
		final Object wrapFn = getRootEnvironment().identifier(new Identifier("java number"));
		return EvalUtil.call(wrapFn, List.list(numberLiteral.getNumber(), numberLiteral.getSuffix()));
	}

	/**
	 * When we encounter an ObjectLiteral from the source, we bind it to the
	 * current lexical environment by wrapping all its slot values with a
	 * Let containing the local variables needed by that expression.
	 */
	@Override
	public Object objectLiteral(ObjectLiteral objectLiteral) {
		TreeMap<String, Slot> slots = TreeMap.treeMap(Ord.stringOrd, objectLiteral.slots
				.map(slot -> P.p(slot.name.id, slot)));
		return new ObjectInstance(slots, this);
	}

    @Override
	public Object slotReference(final SlotReference ref) {
    	final Object target = evaluate(ref.object);
		return EvalUtil.readSlot(target, target, null, ref.slotName.id);
	}

	public static CoreExprEvaluator root(List<P2<Identifier, CoreExpr>> rootBindings) {
		FreeVariableGatherer freeVarGatherer = new FreeVariableGatherer();
		List<P2<Identifier, Binding>> bindings = bindExprsToLazyValues(rootBindings);
		CoreExprEvaluator env = new CoreExprEvaluator(null, bindings, freeVarGatherer);
		return env;
	}

	private static List<P2<Identifier, Binding>> bindExprsToLazyValues(
            List<P2<Identifier, CoreExpr>> rootBindings) {
	    List<P2<Identifier, Binding>> bindings = rootBindings.map(binding -> P.p(binding._1(), Binding.simple(new LazyValue(binding._2()))));
	    return bindings;
    }

	@Override
    public Object let(Let let) {
		List<P2<Identifier, Binding>> bindings = bindExprsToLazyValues(let.bindings);
		CoreExprEvaluator env = child(bindings);
		return env.evaluate(let.body);
    }

	public CoreExprEvaluator child(List<P2<Identifier,Binding>> bindings) {
		return new CoreExprEvaluator(this, bindings, freeVarGatherer);
	}

	/**
	 * Evaluate the expression in the current environment, returning an Object.  Use
	 * EvalUtil to work the the object.  The result may be a thunk - if you need a
	 * concrete value use EvalUtil.force().
	 */
	public Object evaluate(CoreExpr expr) {
		return expr.acceptVisitor(this);
	}

	/**
	 * Evaluate the given expression in the current environment PLUS the
	 * given bindings.
	 */
	public Object evaluate(CoreExpr expr, List<P2<Identifier,Binding>> bindings) {
		return child(bindings).evaluate(expr);
	}

	protected Set<Identifier> freeVars(CoreExpr e) {
	    return freeVarGatherer.analyse(e);
    }

	public static CoreExprEvaluator forSourceFile(String sourceFilePath) {
		return root((new ProjectLoader()).loadLocalAndLibraryBindings(sourceFilePath));
	}

	public static Object eval(String src) {
		return forSourceFile("-").evaluate(CoreExpr.fromString(src));
	}

	/**
	 * Return true if the given expression is a "truthy" falue; that is,
	 * <code>(value && x) == x</code>
	 */
	public boolean isTruthy(CoreExpr expr) {
		return EvalUtil.isTruthy(evaluate(expr));
    }

	@Override
    public Value functionLiteral(FunctionLiteral f) {
		return new FunctionInstance(f, this);
    }
}
