package banjo.eval.coreexpr;

import static java.util.Objects.requireNonNull;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Objects;
import java.util.function.Supplier;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.fraction.Fraction;

import banjo.dom.core.BadCoreExpr;
import banjo.dom.core.BaseFunctionRef;
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
import banjo.eval.ExtendedObject;
import banjo.eval.NotCallable;
import banjo.eval.ProjectLoader;
import banjo.eval.SlotNotFound;
import banjo.eval.UnboundIdentifier;
import banjo.eval.UnresolvedCodeError;
import banjo.eval.Value;
import banjo.eval.util.JavaRuntimeSupport;
import banjo.eval.util.PackageValue;
import banjo.parser.util.SourceFileRange;
import banjo.util.SourceNumber;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.Stream;
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
		bindUnboundThunk(p._2().value);
	}

	private void bindUnboundThunk(Object value) {
	    if(value instanceof LazyCoreExprValue) {
			LazyCoreExprValue thunk = (LazyCoreExprValue)value;
			if(thunk.evaluator == null) {
				thunk.evaluator = this;
				thunk.stack = JavaRuntimeSupport.stack.get();
			}
		}
	    if(value instanceof ExtendedObject) {
	    	ExtendedObject comp = (ExtendedObject)value;
	    	bindUnboundThunk(comp.base);
	    	bindUnboundThunk(comp.extension);
	    }
    }

	public Object lazy(CoreExpr e) {
		return new LazyCoreExprValue(e, this, JavaRuntimeSupport.stack.get());
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
		return new UnresolvedCodeError(badExpr.getMessage(), badExpr.getSourceFileRanges());
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
			return JavaRuntimeSupport.callMethod(target, slotRef.slotName.id, target, null, argsList);
		} else {
			final Object target = evaluate(call.target);
			return JavaRuntimeSupport.call(target, target, null, argsList);
		}
	}

	@Override
	public Object extend(Extend extend) {
		return new ExtendInstance(lazy(extend.base), lazy(extend.extension), extend);
	}

	@Override
	public Object identifier(Identifier id) {
		return getBinding(id)
				.map(b -> b.value)
				.orSome(P.lazy(u -> unboundIdentifier(id)));
    }

	@Override
	public Object baseFunctionRef(BaseFunctionRef baseFunctionRef) {
	    final Identifier id = baseFunctionRef.name;
		final Option<Binding> binding = getBinding(id);
		if(binding.isNone())
			return unboundIdentifier(id);
		return binding.filter(b -> b.baseFunction != null)
	    		.map(b -> b.baseFunction)
	    		.orSome(P.lazy(u -> notAFunctionSelfName(id)));
	}

	protected Object unboundIdentifier(Identifier id) {
	    return new UnboundIdentifier(String.format("Unknown variable '%s'", id.id));
    }

	protected Object notAFunctionSelfName(Identifier id) {
	    return new UnboundIdentifier(String.format("Variable '%s' is not a function's self-recursive name", id.id));
    }

	protected Option<Binding> getBinding(Identifier id) {
		final Option<Binding> binding = this.bindings.get(id);
		if(parent != null && binding.isNone()) return parent.getBinding(id);
		else return binding;
    }

	@Override
	public Object stringLiteral(StringLiteral stringLiteral) {
		return callJavaHelper("string", stringLiteral.string);
	}

	@Override
	public Object inspect(Inspect inspect) {
		return callJavaHelper("mirror", this, inspect);
	}

	@Override
	public Object listLiteral(ListLiteral listLiteral) {
		return callJavaHelper("list", listLiteral.elements.map(this::lazy));
	}

	private Object javaHelpers() {
	    return getRootEnvironment().identifier(new Identifier("java"));
    }

	private Object _callJavaHelper(String name, List<Object> args) {
		return JavaRuntimeSupport.callMethod(javaHelpers(), name, args);
	}

	private Object callJavaHelper(String name, Object arg1) {
		return _callJavaHelper(name, List.single(arg1));
	}

	private Object callJavaHelper(String name, Object arg1, Object arg2) {
		return _callJavaHelper(name, List.list(arg1, arg2));
	}

	@Override
	public Object numberLiteral(NumberLiteral numberLiteral) {
		Number number = numberLiteral.getNumber();
		if(number instanceof SourceNumber) number = ((SourceNumber)number).getValue();
		final Object result = callJavaHelper("number", number);
		return result;
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
		return JavaRuntimeSupport.readSlot(target, target, null, ref.slotName.id);
	}

    static final List<P2<Identifier, Binding>> javaPackages() {
    	return List.single(P.p(new Identifier("java"), Binding.simple(new PackageValue("java"))))
    			.cons(P.p(new Identifier("banjo"), Binding.simple(new PackageValue("banjo"))));
//    	return Stream.stream(Package.getPackages())
//    	.map(PackageValue::new)
//    	.map(p -> P.p(new Identifier(p.getName()), Binding.simple(p)))
//    	.toList();
    }

	public static CoreExprEvaluator root(List<P2<Identifier, CoreExpr>> rootBindings) {
		FreeVariableGatherer freeVarGatherer = new FreeVariableGatherer();
		List<P2<Identifier, Binding>> bindings = mergeRootBindings(javaPackages().append(bindExprsToLazyValues(rootBindings)));
		CoreExprEvaluator env = new CoreExprEvaluator(null, bindings, freeVarGatherer);
		return env;
	}

	/**
	 * Merge same-named bindings using ExtendedObject.  This is only intended for use in the root scope,
	 * because the root bindings automatically "extend" same-named root bindings.
	 *
	 * The result does not maintain the original sort order of the list.
	 *
	 * Currently this assumes all the bindings are "simple" bindings - that is, only have a "value" set.
	 */
	public static List<P2<Identifier, Binding>> mergeRootBindings(
            List<P2<Identifier, Binding>> bindings) {
		final TreeMap<Identifier, Binding> newBindingMap = bindings.foldLeft(CoreExprEvaluator::mergeRootBinding, TreeMap.empty(Identifier.ORD));
		return newBindingMap.toStream().toList();
    }

	/**
	 * Add or merge a single root binding into a map of root bindings.
	 */
	protected static TreeMap<Identifier, Binding> mergeRootBinding(
            TreeMap<Identifier, Binding> bindingMap, P2<Identifier, Binding> binding) {
	    return bindingMap.set(binding._1(),
	    		bindingMap.get(binding._1())
	    		.map(nextBinding -> (Binding) Binding.simple(new ExtendedObject(binding._2().value, nextBinding.value)))
	    		.orSome(binding._2()));
    }

	private static List<P2<Identifier, Binding>> bindExprsToLazyValues(
            List<P2<Identifier, CoreExpr>> rootBindings) {
	    List<P2<Identifier, Binding>> bindings = rootBindings.map(binding -> P.p(binding._1(), Binding.simple(new LazyCoreExprValue(binding._2()))));
	    return bindings;
    }

	@Override
    public Object let(Let let) {
		List<P2<Identifier, Binding>> bindings = bindExprsToLazyValues(let.bindings);
		CoreExprEvaluator env = child(bindings);
		return env.evaluate(let.body);
    }

	public CoreExprEvaluator child(List<P2<Identifier,Binding>> bindings) {
		if(bindings.isEmpty())
			return this;
		return new CoreExprEvaluator(this, bindings, freeVarGatherer);
	}

	/**
	 * Evaluate the expression in the current environment, returning an Object.
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
		return JavaRuntimeSupport.isTruthy(evaluate(expr));
    }

	@Override
    public Value functionLiteral(FunctionLiteral f) {
		return new FunctionInstance(f, this);
    }

	public Object lazy(CoreExpr expr, List<P2<Identifier, Binding>> bindings) {
	    return child(bindings).lazy(expr);
    }
}
