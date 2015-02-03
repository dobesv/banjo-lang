package banjo.eval.coreexpr;

import banjo.dom.BadExpr;
import banjo.dom.core.AnonymousKey;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.core.Extend;
import banjo.dom.core.FreeVariableGatherer;
import banjo.dom.core.Inspect;
import banjo.dom.core.Let;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.MixfixFunctionIdentifier;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.source.Operator;
import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.eval.ProjectLoader;
import banjo.parser.util.SourceFileRange;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.Stream;
import fj.data.TreeMap;

public class CoreExprEvaluator implements CoreExprVisitor<ObjectLiteral> {

	private static final Identifier FAILURE_PROPERTY_ID = new Identifier("//failure");
	private static final ObjectLiteral BASE_FAILURE = new ObjectLiteral(FunctionLiteral.property(FAILURE_PROPERTY_ID, Identifier.TRUE));
	public final CoreExprEvaluator parent;
	public final TreeMap<Key, CoreExpr> bindings;

	public static final TreeMap<Key, CoreExpr> EMPTY_BINDINGS = TreeMap.empty(Key.ORD);

	CoreExprEvaluator(CoreExprEvaluator parent,
			TreeMap<Key, CoreExpr> bindings) {
		super();
		this.parent = parent;
		this.bindings = bindings;
	}

	public CoreExprEvaluator(TreeMap<Key, CoreExpr> bindings) {
		this(null, EMPTY_BINDINGS.union(bindings.toStream().map(p -> P.p(p._1(), bindRoot(p._1(), p._2(), bindings)))));
	}

	public ObjectLiteral failure(String variant, String info) {
		return failure(variant, new StringLiteral(info));
	}

	public ObjectLiteral failure(String variant, CoreExpr info) {
		return bind(new Extend(
				BASE_FAILURE,
				ObjectLiteral.selector(variant, info)
		)).acceptVisitor(this);
	}

	public CoreExprEvaluator getRootEnvironment() {
		final CoreExprEvaluator parent = this.parent;
		return parent == null ? this : parent.getRootEnvironment();
	}

	@Override
	public ObjectLiteral badExpr(BadExpr badExpr) {
		// Non-reducible
		String message = badExpr.getMessage();
		return badExpr(message);
	}

	private ObjectLiteral badExpr(String message) {
		return failure("bad expression", message);
	}

	@Override
	public ObjectLiteral anonymous() {
		return badExpr("ANONYMOUS not expected to be evaluated");
	}

	@Override
	public ObjectLiteral badIdentifier(BadIdentifier badIdentifier) {
		return badExpr(badIdentifier.getMessage());
	}

	static ObjectLiteral sourceFileRangeExpr(SourceFileRange sfr) {
		return new ObjectLiteral(
			FunctionLiteral.property("file", sfr.getSourceFile()),
			FunctionLiteral.property("start line", sfr.getStartLine()),
			FunctionLiteral.property("start column", sfr.getStartColumn()),
			FunctionLiteral.property("start offset", sfr.getFileRange().getStartOffset()),
			FunctionLiteral.property("end line", sfr.getEndLine()),
			FunctionLiteral.property("end column", sfr.getFileRange().getEndColumn()),
			FunctionLiteral.property("end offset", sfr.getFileRange().getEndOffset())
		);
	}

	ObjectLiteral sourceFileRangesExpr(List<SourceFileRange> sfr) {
		return new ListLiteral(sfr.map(CoreExprEvaluator::sourceFileRangeExpr)).acceptVisitor(this);
	}

	@Override
	public ObjectLiteral call(Call call) {
		final CoreExpr callTarget = call.getObject();
		final boolean callNext = call.isCallNext();
		final Key name = call.getName();
		final boolean optionalCall = call.isOptional();
		final List<List<CoreExpr>> argumentLists = call.getArgumentLists();
		final List<SourceFileRange> sourceFileRanges = call.getSourceFileRanges();
		return call(callTarget, name, callNext, optionalCall, argumentLists,
                sourceFileRanges);
	}

	private ObjectLiteral call(CoreExpr targetExpr, final Key name,
            final boolean callNext, final boolean optionalCall,
            final List<List<CoreExpr>> argumentLists,
            final List<SourceFileRange> sourceFileRanges) throws Error {
	    if (callNext) {
			// Tricky stuff - have to pass in a self arg that will
			// result in a call next calling the right next method
			// only within this method.
			throw new Error("TODO");
		}

	    ObjectLiteral targetObject = targetExpr.acceptVisitor(this);

		FunctionLiteral method = targetObject.findMethod(name);

		if (method == null) {
			if (optionalCall) {
				return new ListLiteral(List.nil()).acceptVisitor(this);
			} else {
				if(isFailure(targetObject))
					return targetObject;
				return failure("missing method definition",
						new ObjectLiteral(
								FunctionLiteral.property("method", new StringLiteral(name.toSource())),
								FunctionLiteral.property("target", targetObject),
								FunctionLiteral.property("source", sourceFileRangesExpr(sourceFileRanges))
						));
			}
		}
		final int methodArgListCount = method.getArgumentLists().length();
		List<List<CoreExpr>> argLists = argumentLists.take(methodArgListCount);
		final TreeMap<Key, CoreExpr> selfBinding = method.hasSelfArg() ?
				EMPTY_BINDINGS.set(method.getSelfArg(), targetObject) :
					EMPTY_BINDINGS;
		TreeMap<Key, CoreExpr> newBindings = method
				.getArgumentLists().zip(argLists).foldLeft(
				(bindings1, pair1) -> pair1
						._1()
						.zip(pair1._2())
						.filter(pair2 -> !(pair2._1() instanceof AnonymousKey))
						.foldLeft(
								(bindings2, pair2) -> bindings2.set(
										pair2._1(),
										pair2._2()),
								bindings1), selfBinding);

		CoreExprEvaluator newEnvironment = new CoreExprEvaluator(this, newBindings);

		List<List<Key>> extraMethodArgLists = method.getArgumentLists().drop(argumentLists.length());

		CoreExpr methodBody = extraMethodArgLists.foldRight(
				(argList,b) -> ObjectLiteral.lambda(argList, b),
				method.getBody());

		// TODO Precondition, postcondition
		ObjectLiteral result = methodBody.acceptVisitor(newEnvironment);

		List<List<CoreExpr>> extraCallArgLists = argumentLists.drop(methodArgListCount);
		if(extraCallArgLists.isNotEmpty() && !isFailure(result)) {
			result = call(result, Key.ANONYMOUS, false, false, extraCallArgLists, sourceFileRanges);
		}
		if (optionalCall) {
			// TODO Be more specific about the failure - maybe some will still
			// propagate through ?
			if (isFailure(result)) {
				return new ListLiteral(List.nil()).acceptVisitor(this);
			} else {
				return new ListLiteral(List.single(result)).acceptVisitor(this);
			}
		}
		return result;
    }

	@Override
	public ObjectLiteral extend(Extend extend) {
		ObjectLiteral base = extend.getBase().acceptVisitor(this);
		ObjectLiteral ext = extend.getExtension().acceptVisitor(this);
		return new ObjectLiteral(extend.getSourceFileRanges(), base.getMethods().append(ext.getMethods()));
	}

	@Override
	public ObjectLiteral identifier(Identifier id) {
		return lookup(id);
	}

	private ObjectLiteral lookup(Key id) {
		return this.bindings.get(id)
				.map(e -> e.acceptVisitor(this))
				.orElse(P.lazy(u -> Option.fromNull(parent).map(p -> p.lookup(id))))
	    		.orSome(P.lazy(u -> failure("unbound identifier", id.toString())));
    }

	@Override
	public ObjectLiteral stringLiteral(StringLiteral stringLiteral) {
		return stringLiteral.toConstructionExpression().acceptVisitor(
				getRootEnvironment());
	}

	@Override
	public ObjectLiteral inspect(Inspect inspect) {
		throw new Error("TODO");
	}

	@Override
	public ObjectLiteral listLiteral(ListLiteral listLiteral) {
		return listLiteral.toConstructionExpression().acceptVisitor(
				getRootEnvironment());
	}

	public ObjectLiteral mixfixFunctionIdentifier(MixfixFunctionIdentifier id) {
		return lookup(id);
	}

	@Override
	public ObjectLiteral numberLiteral(NumberLiteral numberLiteral) {
		return numberLiteral.toConstructionExpression().acceptVisitor(getRootEnvironment());
	}

	/**
	 * "Import" all the root bindings into each root binding, if it uses them,
	 * by creating an alias; e.g. <code>foo\/true = true</code> inside a top-level
	 * variable named <code>foo</code>
	 */
	public static CoreExpr bindRoot(Key name, CoreExpr e, TreeMap<Key, CoreExpr> bindings) {
		String idPrefix = ((Identifier)name).id;
		return bindRoot(idPrefix, e, bindings);
	}

	public static CoreExpr bindRoot(String idPrefix, CoreExpr e,
            TreeMap<Key, CoreExpr> bindings) {
	    Set<Key> vars = FreeVariableGatherer.freeVars(e);
		final List<Key> bindingNames = bindings.keys();
		final List<P2<Key, CoreExpr>> aliases = bindingNames
				.map(k -> P.p((Key)new Identifier(idPrefix + ((Identifier)k).id), (CoreExpr)k));
		List<P2<Key,CoreExpr>> newBindings = aliases
				.filter(p -> vars.member(p._1()));
		if(newBindings.isEmpty())
			return e;
		return new Let(newBindings, e);
    }

	/**
	 * Wrap the given expression in a let with the definitions of any variables
	 * in the current environment referenced by that expression.  This captures the
	 * current environment into that expression body.
	 *
	 * If the let would be empty, this returns the original expression as-is.
	 */
	public CoreExpr bind(CoreExpr e) {
		Set<Key> vars = FreeVariableGatherer.freeVars(e);
		final Stream<Option<P2<Key,CoreExpr>>> optionPairs = vars
				.toStream()
				.map(k -> bindings.get(k).map(b -> P.p(k, b)));
		List<P2<Key,CoreExpr>> newBindings = optionPairs
				.filter(Option.isSome_())
				.map(x -> x.some())
				.toList();
		if(newBindings.isEmpty())
			return e;
		return new Let(newBindings, e);
	}

	/**
	 * Bake the current lexical environment into the given method by wrapping its body, precondition, and postcondition
	 * in a new let with any variables required by that expression that are available in scope.
	 */
	public FunctionLiteral bindMethod(FunctionLiteral m) {
		return m.withBody(bind(m.body)).withPrecondition(bind(m.precondition)).withPostcondition(bind(m.postcondition));
	}

	@Override
	public ObjectLiteral objectLiteral(ObjectLiteral objectLiteral) {
		return objectLiteral.withMethods(objectLiteral.getMethods().map(this::bindMethod));
	}

	@Override
	public ObjectLiteral operator(OperatorRef id) {
		throw new Error("Should not happen");
	}

	public static CoreExprEvaluator root(TreeMap<Key, CoreExpr> rootDefs) {
		return new CoreExprEvaluator(rootDefs);
	}

	@Override
    public ObjectLiteral let(Let let) {
		return let.body.acceptVisitor(new CoreExprEvaluator(this, EMPTY_BINDINGS.union(let.bindings)));
    }

	public static boolean isFailure(ObjectLiteral x) {
		return x.findMethod(FAILURE_PROPERTY_ID) != null;
	}


	public ObjectLiteral evaluate(CoreExpr expr) {
		return bindRoot("/temp", expr, bindings).acceptVisitor(this);
	}

	public static CoreExprEvaluator forSourceFile(String sourceFilePath) {
		return new CoreExprEvaluator(ProjectLoader.loadLocalAndLibraryBindings(sourceFilePath));
	}

	public static ObjectLiteral eval(String src) {
		return forSourceFile("-").evaluate(CoreExpr.fromString(src));
	}

	/**
	 * Return true if the given expression is a "truthy" falue; that is,
	 * <code>(value && x) == x</code>
	 */
	public static boolean isTruthy(CoreExpr result) {
		ObjectLiteral truthyMarker = ObjectLiteral.selector("truthy");
		final ObjectLiteral checkResult = new CoreExprEvaluator(EMPTY_BINDINGS).evaluate(
			new Call(result, Operator.LOGICAL_AND.getMethodIdentifier(), truthyMarker)
		);
		return checkResult.equals(truthyMarker);
    }

}
