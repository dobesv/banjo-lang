package banjo.eval.coreexpr;

import java.util.function.Supplier;

import banjo.dom.BadExpr;
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
import banjo.dom.core.SlotReference;
import banjo.dom.source.Operator;
import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.eval.ProjectLoader;
import banjo.parser.util.SourceFileRange;
import fj.F;
import fj.F0;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.Stream;
import fj.data.TreeMap;

public class CoreExprEvaluator implements CoreExprVisitor<CoreExpr> {

	private static final Identifier FAILURE_PROPERTY_ID = new Identifier("//failure");
	private static final ObjectLiteral BASE_FAILURE = new ObjectLiteral(P.p(FAILURE_PROPERTY_ID, Identifier.TRUE));
	public final CoreExprEvaluator parent;
	public final TreeMap<Identifier, CoreExpr> bindings;

	public static final TreeMap<Identifier, CoreExpr> EMPTY_BINDINGS = TreeMap.empty(Identifier.ORD);

	// Cache.  The cache keys and values must not have free variables!
	private TreeMap<CoreExpr,CoreExpr> cache = TreeMap.empty(CoreExpr.coreExprOrd);

	CoreExprEvaluator(CoreExprEvaluator parent,
			TreeMap<Identifier, CoreExpr> bindings) {
		super();
		this.parent = parent;
		this.bindings = bindings;
	}

	public CoreExprEvaluator(TreeMap<Identifier, CoreExpr> bindings) {
		this(null, bindings);
	}

	public CoreExpr failure(String variant, String info) {
		return failure(variant, new StringLiteral(info));
	}

	public CoreExpr failure(String variant, CoreExpr info) {
		return bind(new Extend(
				BASE_FAILURE,
				FunctionLiteral.selector(variant, info)
		));
	}

	public CoreExprEvaluator getRootEnvironment() {
		final CoreExprEvaluator parent = this.parent;
		return parent == null ? this : parent.getRootEnvironment();
	}

	@Override
	public CoreExpr badExpr(BadCoreExpr badExpr) {
		return (BadCoreExpr) badExpr;
	}

	private CoreExpr badExpr(String message) {
		return failure("bad expression", message);
	}

	@Override
	public CoreExpr badIdentifier(BadIdentifier badIdentifier) {
		return badIdentifier;
	}

	@SuppressWarnings("unchecked")
    static CoreExpr sourceFileRangeExpr(SourceFileRange sfr) {
		return new ObjectLiteral(List.list(
			ObjectLiteral.slot("file", sfr.getSourceFile()),
			ObjectLiteral.slot("start line", sfr.getStartLine()),
			ObjectLiteral.slot("start column", sfr.getStartColumn()),
			ObjectLiteral.slot("start offset", sfr.getFileRange().getStartOffset()),
			ObjectLiteral.slot("end line", sfr.getEndLine()),
			ObjectLiteral.slot("end column", sfr.getFileRange().getEndColumn()),
			ObjectLiteral.slot("end offset", sfr.getFileRange().getEndOffset())
		));
	}

	CoreExpr sourceFileRangesExpr(List<SourceFileRange> sfr) {
		return new ListLiteral(sfr.map(CoreExprEvaluator::sourceFileRangeExpr)).acceptVisitor(this);
	}

	public CoreExpr updateCache(CoreExpr k, CoreExpr v) {
		cache = cache.set(k, v);
		if(parent != null)
			parent.updateCache(k, v);
		return v;
	}
	public CoreExpr useCache(CoreExpr e, F0<CoreExpr> gen) {
		CoreExpr cacheKey = bind(e);
		return cache.get(cacheKey).orSome(P.lazy(u -> updateCache(cacheKey, gen.f())));
	}

	@Override
	public CoreExpr call(Call call) {
		return useCache(call, () -> call(call.target, call.args, call.getSourceFileRanges()));
	}

	private CoreExpr call(CoreExpr targetExpr, List<CoreExpr> args, List<SourceFileRange> sourceFileRanges) throws Error {
		// Force the target to an irreducible form
		CoreExpr target = force(targetExpr);


		FunctionLiteral func = getCallable(targetExpr).toNull();

		if (func == null) {
			if(isFailure(target))
				return target;
			return failure("target is not a function",
					new ObjectLiteral(List.list(
							ObjectLiteral.slot("target", targetExpr),
							ObjectLiteral.slot("source", sourceFileRangesExpr(sourceFileRanges))
					))
			);
		}

		// Force argument list to match the length of the target function - drop extras, insert "failure" objects for missing arguments
		List<CoreExpr> matchedArgs = args.append(func.args.drop(args.length()).map(name -> failure("missing argument", name.id))).take(func.args.length());
		List<P2<Identifier, CoreExpr>> newBindings =
				func.args.zip(matchedArgs).append(bindings.toStream().toList())
				.cons(P.p(Identifier.__SELF, target)); // Functions get themselves as __self in a call, slots get the target object as __self; they must alias __self pronto

		return bind(func.body, newBindings);
    }

	private Option<FunctionLiteral> getCallable(CoreExpr target) {
		final CoreExprEvaluator evalVisitor = this;
	    return target.acceptVisitor(new CoreExprVisitor<Option<FunctionLiteral>>() {
	    	@Override
	    	public Option<FunctionLiteral> functionLiteral(FunctionLiteral f) {
	    	    return Option.some(f);
	    	}

	    	@Override
	    	public Option<FunctionLiteral> call(Call n) {
	    		return reducible(n);
	    	}

			private Option<FunctionLiteral> reducible(CoreExpr n) {
	            return n.acceptVisitor(evalVisitor).acceptVisitor(this);
            }

	    	@Override
	    	public Option<FunctionLiteral> extend(Extend n) {
	    		return n.getExtension().acceptVisitor(this).orElse(P.lazy(u -> n.getBase().acceptVisitor(this)));
	    	}

	    	@Override
	    	public Option<FunctionLiteral> identifier(Identifier n) {
	    	    return reducible(n);
	    	}

	    	@Override
	    	public Option<FunctionLiteral> let(Let let) {
	    	    return reducible(let);
	    	}

	    	@Override
	    	public Option<FunctionLiteral> listLiteral(ListLiteral n) {
	    	    return reducible(n);
	    	}

	    	@Override
	    	public Option<FunctionLiteral> inspect(Inspect n) {
	    	    return reducible(n);
	    	}

	    	@Override
	    	public Option<FunctionLiteral> slotReference(SlotReference slotReference) {
	    	    return reducible(slotReference);
	    	}

	    	@Override
	    	public Option<FunctionLiteral> stringLiteral(StringLiteral n) {
	    	    return reducible(n);
	    	}

			@Override
            public Option<FunctionLiteral> badExpr(BadCoreExpr badExpr) {
	    	    return Option.none();
            }

			@Override
            public Option<FunctionLiteral> numberLiteral(NumberLiteral numberLiteral) {
				// Numbers are not callable, for now
	    	    return Option.none();
            }

			@Override
            public Option<FunctionLiteral> operator(OperatorRef operatorRef) {
	    	    return Option.none();
            }

			@Override
            public Option<FunctionLiteral> objectLiteral(ObjectLiteral objectLiteral) {
				// ObjectLiteral is not callable
	    	    return Option.none();
            }

			@Override
            public Option<FunctionLiteral> badIdentifier(BadIdentifier badIdentifier) {
	            return Option.none();
            }
		});
    }

	private Option<CoreExpr> getSlot(CoreExpr object, Identifier slotName, F<CoreExpr,CoreExpr> f) {
		CoreExprEvaluator evalVisitor = this;
		return object.acceptVisitor(new CoreExprVisitor<Option<CoreExpr>>() {
			@Override
			public Option<CoreExpr> badExpr(BadCoreExpr badExpr) {
			    return Option.some((BadCoreExpr)badExpr);
			}

			@Override
			public Option<CoreExpr> badIdentifier(BadIdentifier badIdentifier) {
			    return Option.some(badIdentifier);
			}

			private Option<CoreExpr> reducible(CoreExpr n) {
	            final CoreExpr e = n.acceptVisitor(evalVisitor);
	            if(e.eql(n) && !isFailure(e)) {
	            	throw new Error("Evaluation was a no-op ? " + n.acceptVisitor(evalVisitor));
	            }
				return e.acceptVisitor(this);
            }

			@Override
			public Option<CoreExpr> call(Call n) {
			    return reducible(n);
			}

	    	@Override
	    	public Option<CoreExpr> extend(Extend n) {
	    		return n.getExtension().acceptVisitor(this).orElse(P.lazy(u -> n.getBase().acceptVisitor(this)));
	    	}

	    	@Override
	    	public Option<CoreExpr> identifier(Identifier n) {
	    	    return reducible(n);
	    	}

	    	@Override
	    	public Option<CoreExpr> let(Let let) {
	    	    return reducible(let);
	    	}

	    	@Override
	    	public Option<CoreExpr> listLiteral(ListLiteral n) {
	    	    return reducible(n);
	    	}

	    	@Override
	    	public Option<CoreExpr> inspect(Inspect n) {
	    	    return reducible(n);
	    	}

	    	@Override
	    	public Option<CoreExpr> slotReference(SlotReference slotReference) {
	    	    return reducible(slotReference);
	    	}

	    	@Override
	    	public Option<CoreExpr> stringLiteral(StringLiteral n) {
	    	    return reducible(n);
	    	}

			@Override
            public Option<CoreExpr> numberLiteral(NumberLiteral n) {
	    	    return reducible(n);
            }

			@Override
            public Option<CoreExpr> operator(OperatorRef operatorRef) {
	    	    return Option.none();
            }

			@Override
            public Option<CoreExpr> objectLiteral(ObjectLiteral objectLiteral) {
				return objectLiteral.findMethod(slotName).map(f);
            }

			@Override
            public Option<CoreExpr> functionLiteral(FunctionLiteral f) {
				// No slots in a function literal, I guess
	            return Option.none();
            }

		});
    }


	@Override
	public CoreExpr extend(Extend extend) {
		CoreExpr base = extend.getBase().acceptVisitor(this);
		CoreExpr ext = extend.getExtension().acceptVisitor(this);
		if(base instanceof ObjectLiteral && ext instanceof ObjectLiteral) {
			ObjectLiteral baseObj = (ObjectLiteral) base;
			ObjectLiteral extObj = (ObjectLiteral) ext;
			// TODO Inject super-methods into overrides as appropriate
			return new ObjectLiteral(
					baseObj.getSourceFileRanges().append(extObj.getSourceFileRanges()),
					baseObj.getSlots().append(extObj.getSlots()));
		}
		return new Extend(extend.getSourceFileRanges(), base, ext);
	}

	@Override
	public CoreExpr identifier(Identifier id) {
		return lookup(id);
    }

	private CoreExpr lookup(Identifier id) {
	    return getBinding(id)
	    		.orSome(P.lazy(u -> unboundIdentifier(id)));
    }

	protected CoreExpr unboundIdentifier(Identifier id) {
	    return failure("unbound identifier", id.toString());
    }

	protected Option<CoreExpr> getBinding(Identifier id) {
		final Option<CoreExpr> binding = this.bindings.get(id);
		if(parent != null) return binding.orElse(P.lazy(u -> parent.getBinding(id)));
		else return binding;
    }

	@Override
	public CoreExpr stringLiteral(StringLiteral stringLiteral) {
		return useCache(stringLiteral, () -> stringLiteral.toConstructionExpression().acceptVisitor(
				getRootEnvironment()));
	}

	@Override
	public CoreExpr inspect(Inspect inspect) {
		throw new Error("TODO");
	}

	@Override
	public CoreExpr listLiteral(ListLiteral listLiteral) {
		return useCache(listLiteral, () -> listLiteral.toConstructionExpression().acceptVisitor(
				getRootEnvironment()));
	}

	@Override
	public CoreExpr numberLiteral(NumberLiteral numberLiteral) {
		return useCache(numberLiteral, () -> numberLiteral.toConstructionExpression().acceptVisitor(getRootEnvironment()));
	}

	/**
	 * Wrap the given expression in a let with the definitions of any variables
	 * in the current environment referenced by that expression.  This captures the
	 * current environment into that expression body.
	 *
	 * If the let would be empty, this returns the original expression as-is.
	 */
	public CoreExpr bind(CoreExpr e) {
		if(e instanceof Identifier) {
			return e.acceptVisitor(this);
		}
		Set<Identifier> vars = FreeVariableGatherer.freeVars(e);
		return bind(e, vars);
	}

	private CoreExpr bind(CoreExpr e, Set<Identifier> vars) {
	    final Stream<Option<P2<Identifier,CoreExpr>>> optBindings = vars
				.toStream()
				.map(this::getBindingPair);
		final List<P2<Identifier,CoreExpr>> newBindings = optBindings
				.filter(Option.isSome_())
				.map(o -> o.some())
				.toList();
		return bind(e, newBindings);
    }

	protected CoreExpr bind(CoreExpr e, final List<P2<Identifier, CoreExpr>> newBindings) {
	    if(newBindings.isEmpty())
			return e;
		if(e instanceof Let) {
			return ((Let)e).addBindings(newBindings);
		}
		return new Let(newBindings, e);
    }

	protected CoreExpr bind(CoreExpr e, Identifier name, CoreExpr value) {
		return bind(e, List.single(P.p(name, value)));
	}

	protected Option<P2<Identifier, CoreExpr>> getBindingPair(Identifier k) {
	    return getBinding(k).map(v -> P.p(k, v));
    }

	/**
	 * Bake the current lexical environment into the given slot value by wrapping it in a Let
	 * containing definitions of all the variables that it uses.
	 */
	public P2<Identifier, CoreExpr> bind(P2<Identifier, CoreExpr> binding) {
		return P.p(binding._1(), this.bind(binding._2()));
	}

	/**
	 * When we encounter an ObjectLiteral from the source, we bind it to the
	 * current lexical environment by wrapping all its slot values with a
	 * Let containing the local variables needed by that expression.
	 */
	@Override
	public CoreExpr objectLiteral(ObjectLiteral objectLiteral) {
		return objectLiteral.withSlots(objectLiteral.getSlots().map(this::bind));
	}

	@SuppressWarnings("unchecked")
    @Override
	public CoreExpr slotReference(final SlotReference ref) {
		CoreExpr object = force(ref.object);
		return useCache(ref,
				() -> getSlot(object, ref.slotName, slotValue -> bind(bind(slotValue), Identifier.__SELF, bind(object)))
				      .orSome(P.lazy(u -> failure("no such slot", new ObjectLiteral(List.<P2<Identifier,CoreExpr>>list(
						ObjectLiteral.slot("slot name", ref.slotName.toString()),
						ObjectLiteral.slot("object", object)
				      )))))
		);
	}

	@Override
	public CoreExpr operator(OperatorRef id) {
		throw new Error("Should not happen");
	}

	public static CoreExprEvaluator root(TreeMap<Identifier, CoreExpr> rootDefs) {
		return new CoreExprEvaluator(rootDefs);
	}

	@Override
    public CoreExpr let(Let let) {
		return let.body.acceptVisitor(new CoreExprEvaluator(this, EMPTY_BINDINGS.union(let.bindings.map(P2.map2_(this::bind)))));
    }

	public boolean isFailure(CoreExpr target) {
		return getSlot(target, FAILURE_PROPERTY_ID, x -> FAILURE_PROPERTY_ID).isSome();
	}

	/**
	 * Evaluate the expression in the current environment.
	 *
	 * The result will be a free-standing value that doesn't depend on
	 * this environment any more.
	 */
	public CoreExpr evaluate(CoreExpr expr) {
		return expr.acceptVisitor(this);
	}

	public static CoreExprEvaluator forSourceFile(String sourceFilePath) {
		return new CoreExprEvaluator((new ProjectLoader()).loadLocalAndLibraryBindings(sourceFilePath));
	}

	public static CoreExpr eval(String src) {
		return forSourceFile("-").evaluate(CoreExpr.fromString(src));
	}

	/**
	 * Return true if the given expression is a "truthy" falue; that is,
	 * <code>(value && x) == x</code>
	 */
	public boolean isTruthy(CoreExpr result) {
		FunctionLiteral truthyMarker = FunctionLiteral.selector("truthy");
		final CoreExpr checkResult = evaluate(evaluate(
			Call.binaryOp(result, Operator.LOGICAL_AND, truthyMarker)
		));
		return checkResult == truthyMarker;
    }

	@Override
    public CoreExpr functionLiteral(FunctionLiteral f) {
		Set<Identifier> freeVars = FreeVariableGatherer.freeVars(f);
		return f.withBody(bind(f.body, freeVars));
    }

	public Stream<P2<Identifier, CoreExpr>> allBindings() {
		if(parent == null) {
			return bindings.toStream();
		} else {
			return parent.allBindings().append(bindings.toStream());
		}
	}
	/**
	 * Replace all expressions that match a binding in this environment
	 * with their identifier.  Helps to compact a source tree prior
	 * to printing it.
	 */
	public CoreExpr simplify(CoreExpr value) {
		TreeMap<CoreExpr, Identifier> revMap = TreeMap.treeMap(CoreExpr.coreExprOrd, getRootEnvironment().bindings.toStream().map(P2.swap_()).toList());

	    return _simplify(value, revMap);
    }

	private CoreExpr _simplify(CoreExpr value, TreeMap<CoreExpr, Identifier> revMap) {

	    final CoreExprVisitor<CoreExpr> simplifier = new CoreExprVisitor<CoreExpr>() {

	    	public CoreExpr simplifyChild(CoreExpr e) {
	    		return _simplify(e, revMap);
	    	}

			@Override
            public CoreExpr badExpr(BadCoreExpr badExpr) {
	            return (BadCoreExpr)badExpr;
            }

			@Override
            public CoreExpr stringLiteral(StringLiteral stringLiteral) {
	            return stringLiteral;
            }

			@Override
            public CoreExpr numberLiteral(NumberLiteral numberLiteral) {
	            return numberLiteral;
            }

			@Override
            public CoreExpr identifier(Identifier identifier) {
	            return identifier;
            }

			@Override
            public CoreExpr operator(OperatorRef operatorRef) {
	            return operatorRef;
            }

			@Override
            public CoreExpr call(Call call) {
				return new Call(call.getSourceFileRanges(), simplifyChild(call.target), call.args.map(this::simplifyChild));
            }

			@Override
            public CoreExpr objectLiteral(ObjectLiteral objectLiteral) {
				return new ObjectLiteral(objectLiteral.getSourceFileRanges(), objectLiteral.getSlots().map(p -> P.p(p._1(), simplifyChild(p._2()))));
            }

			@Override
            public CoreExpr listLiteral(ListLiteral listLiteral) {
	            return new ListLiteral(listLiteral.getSourceFileRanges(), listLiteral.elements.map(this::simplifyChild));
            }

			@Override
            public CoreExpr badIdentifier(BadIdentifier badIdentifier) {
	            return badIdentifier;
            }

			@Override
            public CoreExpr inspect(Inspect inspect) {
	            return new Inspect(inspect.getSourceFileRanges(), simplifyChild(inspect.target));
            }

			@Override
            public CoreExpr extend(Extend extend) {
	            return new Extend(extend.getSourceFileRanges(), extend.getBase().acceptVisitor(this), extend.getExtension().acceptVisitor(this));
            }

			@Override
            public CoreExpr let(Let let) {
	            Let newLet = new Let(let.getSourceFileRanges(),
	            		let.bindings
	            		.map(p -> P.p(p._1(), simplifyChild(p._2())))
	            		.filter(p -> !p._1().eql(p._2()))
	            		.filter(p -> !p._1().eql(Identifier.UNDERSCORE) && !p._1().eql(Identifier.USAGE_EXAMPLES))
	            		, simplifyChild(let.body));
	            if(newLet.bindings.isEmpty())
	            	return newLet.body;
	            return newLet;
            }

			@Override
            public CoreExpr functionLiteral(FunctionLiteral f) {
	            return new FunctionLiteral(f.args, simplifyChild(f.body));
            }

			@Override
            public CoreExpr slotReference(SlotReference slotReference) {
	            return new SlotReference(slotReference.getSourceFileRanges(), simplifyChild(slotReference.object), slotReference.slotName);
            }
		};
		return revMap.get(value).map(x -> (CoreExpr)x).orSome(P.lazy(() -> value.acceptVisitor(simplifier)));
    }

	public boolean isReducible(CoreExpr e) {
		return e.acceptVisitor(new CoreExprVisitor<Boolean>() {
			@Override
			public Boolean badExpr(BadCoreExpr badExpr) {
			    return false;
			}

			@Override
			public Boolean badIdentifier(BadIdentifier badIdentifier) {
			    return false;
			}

			@Override
			public Boolean call(Call call) {
			    return true;
			}

			@Override
			public Boolean extend(Extend extend) {
			    return extend.getBase().acceptVisitor(this) || extend.getExtension().acceptVisitor(this);
			}

			@Override
			public Boolean functionLiteral(FunctionLiteral f) {
			    return false;
			}

			@Override
			public Boolean identifier(Identifier identifier) {
			    return true;
			}

			@Override
			public Boolean inspect(Inspect inspect) {
			    return inspect.target.acceptVisitor(this);
			}

			@Override
			public Boolean let(Let let) {
			    return let.body.acceptVisitor(this);
			}

			@Override
			public Boolean listLiteral(ListLiteral listLiteral) {
			    return listLiteral.elements.exists(x -> x.acceptVisitor(this));
			}

			@Override
			public Boolean numberLiteral(NumberLiteral numberLiteral) {
			    return false;
			}

			@Override
			public Boolean objectLiteral(ObjectLiteral objectLiteral) {
			    return false;
			}

			@Override
			public Boolean operator(OperatorRef operatorRef) {
			    return false;
			}

			@Override
			public Boolean slotReference(SlotReference slotReference) {
			    return true;
			}

			@Override
			public Boolean stringLiteral(StringLiteral stringLiteral) {
			    return false;
			}
		});
	}
	/**
	 * Keep evaluating the expression until it is atomic - that is, not
	 * a call or slot reference.
	 */
	public CoreExpr force(CoreExpr e) {
		if(isReducible(e)) {
			return force(e.acceptVisitor(this));
		}
		return bind(e);
	}
}
