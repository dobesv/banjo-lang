package banjo.desugar;

import static banjo.parser.util.Check.nonNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.dom.Expr;
import banjo.dom.core.BadCoreExpr;
import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.Extend;
import banjo.dom.core.Inspect;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.Method;
import banjo.dom.core.MethodParamDecl;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.source.BadSourceExpr;
import banjo.dom.source.BaseSourceExprVisitor;
import banjo.dom.source.BinaryOp;
import banjo.dom.source.EmptyExpr;
import banjo.dom.source.Operator;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.SourceExprVisitor;
import banjo.dom.source.UnaryOp;
import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Ellipsis;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.parser.util.DesugarMap;
import banjo.parser.util.ExprOrd;
import banjo.parser.util.FileRange;
import banjo.parser.util.SourceMap;
import fj.F;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.data.Set;
import fj.data.TreeMap;

public class BanjoDesugarer {
	final TreeMap<Expr, CoreExpr> oldCache;
	final TreeMap<Expr, CoreExpr> newCache;
	protected final DesugarMap desugarMap;
	final int counter;

	public static class DesugarResult<T> extends BanjoDesugarer {
		final SourceExpr sourceExpr;
		final T value;
		public DesugarResult(T expr, SourceExpr sourceExpr, TreeMap<Expr, CoreExpr> oldCache, TreeMap<Expr, CoreExpr> newCache, DesugarMap desugarMap, int counter) {
			super(oldCache, newCache, desugarMap, counter);
			this.value = expr;
			this.sourceExpr = sourceExpr;
		}

		public DesugarResult(T result, SourceExpr sourceExpr, BanjoDesugarer ds) {
			this(result, sourceExpr, ds.oldCache, ds.newCache, ds.getDesugarMap(), ds.counter + 1);
		}

		public T getValue() {
			return this.value;
		}

		public SourceExpr getSourceExpr() {
			return this.sourceExpr;
		}

		public DesugarResult<CoreExpr> redesugar(SourceExpr expr, SourceMap sourceMaps) {
			return new BanjoDesugarer(this.newCache, EMPTY_CACHE, EMPTY_DESUGAR_MAP, 1).desugar(expr);
		}

		public TreeMap<FileRange, Set<BadExpr>> getProblems(SourceMap sourceMap) {
			return getProblems(sourceMap, EMPTY_ERROR_MAP);
		}

		public TreeMap<FileRange, Set<BadExpr>> getProblems(final SourceMap sourceMap, TreeMap<FileRange, Set<BadExpr>> errors) {
			for(final P2<CoreExpr,Set<SourceExpr>> p : this.desugarMap.getCoreExprToSourceExpr()) {
				final TreeMap<FileRange, Set<BadExpr>> tmpErrors = errors;
				errors = nonNull(p._1().acceptVisitor(new BaseCoreExprVisitor<TreeMap<FileRange, Set<BadExpr>>>() {
					@Override
					@Nullable
					public TreeMap<FileRange, Set<BadExpr>> badExpr(BadCoreExpr n) {
						// TODO Auto-generated method stub
						TreeMap<FileRange, Set<BadExpr>> newErrors = tmpErrors;
						for(final SourceExpr sourceExpr : p._2()) {
							for(final FileRange range : sourceMap.get(sourceExpr)) {
								newErrors = newErrors.set(range, newErrors.get(range).orSome(Set.<BadExpr>empty(ExprOrd.<BadExpr>exprOrd()).insert(n)));
							}
						}
						return newErrors;
					}

					@Override
					@Nullable
					public TreeMap<FileRange, Set<BadExpr>> fallback(CoreExpr unsupported) {
						return tmpErrors;
					}
				}));
			}
			return errors;
		}
	}

	@SuppressWarnings("null")
	public static final TreeMap<FileRange, Set<BadExpr>> EMPTY_ERROR_MAP = TreeMap.empty(Ord.<FileRange>comparableOrd());
	@SuppressWarnings("null")
	public static final TreeMap<Expr, CoreExpr> EMPTY_CACHE = TreeMap.empty(Ord.<Expr>comparableOrd());
	@SuppressWarnings("null")
	static final fj.data.Set<SourceExpr> EMPTY_SOURCE_EXPR_SET = fj.data.Set.empty(SourceExpr.ORD);
	static final DesugarMap EMPTY_DESUGAR_MAP = new DesugarMap();

	<TT> DesugarResult<TT> withValue(TT result, SourceExpr sourceExpr) {
		return new DesugarResult<TT>(result, sourceExpr, this);
	}

	@SuppressWarnings("null")
	protected DesugarResult<CoreExpr> withDesugared(SourceExpr sourceExpr, CoreExpr expr) {
		final TreeMap<Expr, CoreExpr> newCache = this.newCache.set(sourceExpr, expr);
		final DesugarMap newDesugarMap = this.getDesugarMap().insert(expr, sourceExpr);
		return new DesugarResult<CoreExpr>(expr, sourceExpr, this.oldCache, newCache, newDesugarMap, this.counter+1);
	}

	@SuppressWarnings("null")
	protected DesugarResult<Key> withDesugared(SourceExpr sourceExpr, Key expr) {
		final TreeMap<Expr, CoreExpr> newCache = this.newCache.set(sourceExpr, expr);
		final DesugarMap newDesugarMap = this.getDesugarMap().insert(expr, sourceExpr);
		return new DesugarResult<Key>(expr, sourceExpr, this.oldCache, newCache, newDesugarMap, this.counter+1);
	}

	protected DesugarResult<Method> withDesugared(SourceExpr signatureSourceExpr, SourceExpr bodySourceExpr, Method method) {
		final DesugarMap newDesugarMap = this.getDesugarMap().insert(method, signatureSourceExpr, bodySourceExpr);
		return new DesugarResult<Method>(method, signatureSourceExpr, this.oldCache, this.newCache, newDesugarMap, this.counter+1);
	}

	protected DesugarResult<MethodParamDecl> withDesugared(SourceExpr sourceExpr, MethodParamDecl methodParamDecl) {
		final DesugarMap newDesugarMap = this.getDesugarMap().insert(methodParamDecl, sourceExpr);
		return new DesugarResult<MethodParamDecl>(methodParamDecl, sourceExpr, this.oldCache, this.newCache, newDesugarMap, this.counter+1);
	}

	/**
	 * Desugarer with a source map
	 */
	public BanjoDesugarer(SourceMap sourceMaps) {
		this(EMPTY_CACHE, EMPTY_CACHE, EMPTY_DESUGAR_MAP, 1);
	}


	/**
	 * Desugarer with sourcemaps and a starting cache value
	 */
	public BanjoDesugarer(TreeMap<Expr, CoreExpr> oldCache, TreeMap<Expr, CoreExpr> newCache, DesugarMap desugarMap, int counter) {
		super();
		this.oldCache = oldCache;
		this.newCache = newCache;
		this.desugarMap = desugarMap;
		this.counter = counter;
	}


	/**
	 * Coming in we have a tree of basically just unary and binary operations and parens and atoms.
	 * 
	 * Desugaring changes that into function calls, projections, object literals, etc..
	 */
	public DesugarResult<CoreExpr> desugar(SourceExpr rootNode) {
		return expr(rootNode);
	}

	public DesugarResult<CoreExpr> expr(SourceExpr sourceExpr) {
		final CoreExpr cachedResult = this.newCache.get(sourceExpr).toNull();
		if(cachedResult != null) {
			// TODO What about the desugar mapping for all the sub-expressions ?
			return withDesugared(sourceExpr, cachedResult);
		}
		final CoreExpr oldCachedResult = this.oldCache.get(sourceExpr).toNull();
		if(oldCachedResult != null) {
			return withDesugared(sourceExpr, oldCachedResult);
		}


		return nonNull(sourceExpr.acceptVisitor(new DesugarVisitor()));
	}

	/**
	 * Desugar a few exprs at once.  The state is accumulated from one to the next so that the
	 * last element of the array has the accumulated cache from the previous desugarings.
	 */
	public DesugarResult<CoreExpr[]> exprs(SourceExpr parent, SourceExpr ... sourceExprs) {
		final CoreExpr[] results = new CoreExpr[sourceExprs.length];
		BanjoDesugarer desugarer = this;
		for(int i=0; i < sourceExprs.length; i++) {
			@SuppressWarnings("null")
			final DesugarResult<CoreExpr> resultDs = desugarer.expr(sourceExprs[i]);
			desugarer = resultDs;
			results[i] = resultDs.getValue();
		}
		return new DesugarResult<CoreExpr[]>(results, parent, desugarer);
	}

	private DesugarResult<CoreExpr> comparison(BinaryOp op) {
		final DesugarResult<CoreExpr[]> ds = exprs(op, op.getLeft(), op.getRight());
		final CoreExpr[] operands = ds.getValue();
		final CoreExpr cmp = new Call(nonNull(operands[0]), new Identifier(nonNull(Operator.CMP.getOp())), nonNull(operands[1]));
		boolean invertCheck;
		String checkField;
		switch(op.getOperator()) {
		case GT: invertCheck = false; checkField = ">"; break;
		case GE: invertCheck = true; checkField = "<"; break;
		case LT: invertCheck = false; checkField = "<"; break;
		case LE: invertCheck = true; checkField = ">"; break;
		default: throw new Error();
		}
		CoreExpr check = new Call(cmp, new Identifier(checkField));
		if(invertCheck)
			check = new Call(check, new Identifier(nonNull(Operator.NOT.getOp())));
		return ds.withDesugared(op, check);
	}

	/**
	 * Projection.
	 * 
	 * @see Projection
	 * @param op BinaryOp describing the projection (having PROJECTION as its operator)
	 * @param sourceOffset Source offset to the start of op
	 */
	protected DesugarResult<CoreExpr> projection(BinaryOp op) {
		return projection(op, op.getLeft(), op.getRight());
	}

	/**
	 * Desugar a projection.  The most basic form of projection is <code>foo.bar</code> which desugars
	 * the property <code>bar</code> in object <code>foo</code>, translating into a call <code>foo.bar()</code>.
	 * 
	 * More complex projections are possible also, however.  <code>foo.{bar,baz}</code> will translate into
	 * <code>{bar:foo.bar, baz:foo.baz}</code> and <code>foo.[bar,baz]</code> will translate into
	 * <code>[foo.bar(), foo.baz()]</code>.  Renames are possible when selecting fields for a new object,
	 * so <code>foo.{bar:baz,baz:bar}</code> would swap the fields as if <code>{bar:foo.baz, baz:foo.bar}</code>
	 * were written.
	 * 
	 * @param sourceExpr Root source expression for the projection.
	 * @param objectSourceOffset Absolute file offset of the start of the left-hand side of the projection
	 * @param baseCoreExpr Desugared left-hand side of the projection; the "base" object we're projecting from
	 * @param projectionCoreExpr Desugared right-hand side of the project; the description of the projection
	 * @return A new CoreExpr, possibly with errors
	 */
	protected DesugarResult<CoreExpr> projection(final SourceExpr sourceExpr, final SourceExpr objectExpr, final SourceExpr expr) {
		final DesugarResult<CoreExpr> objectDs = expr(objectExpr);
		return objectDs.projection(sourceExpr, objectDs.getValue(), expr);
	}

	protected DesugarResult<CoreExpr> projection(final SourceExpr sourceExpr, final CoreExpr objectCoreExpr, final SourceExpr expr) {
		return nonNull(expr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
			@Override
			@Nullable
			public DesugarResult<CoreExpr> key(Key key) {
				return withDesugared(sourceExpr, new Call(objectCoreExpr, key));
			}

			@Override
			@Nullable
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				// TODO: Implement list / object projections
				return withDesugared(sourceExpr, new BadCoreExpr("Expected identifier"));
			}

		}));
	}

	/**
	 * Optional projection means we have to check whether the field is there or not
	 * before calling.  If the field is not there, return none.
	 * 
	 * <pre>x.?foo == x#["foo"] && x.foo</pre>
	 * <pre>x.?foo(bar) == x#["foo"] && x.foo(bar)</pre>
	 */
	protected DesugarResult<CoreExpr> optionalProjection(BinaryOp op) {
		return optionalProjection(op, op.getLeft(), op.getRight());
	}

	public DesugarResult<CoreExpr> optionalProjection(SourceExpr sourceExpr, final SourceExpr targetSourceExpr, final SourceExpr projectionSourceExpr) {
		final DesugarResult<CoreExpr> targetDs = expr(targetSourceExpr);
		final CoreExpr target = targetDs.getValue();
		final DesugarResult<Key> methodNameDs = targetDs.expectIdentifier(projectionSourceExpr); // Only simple projections for now
		final Inspect metadata = new Inspect(target);
		final Call lookup = new Call(metadata, Method.LOOKUP_METHOD_NAME, new StringLiteral(methodNameDs.getValue().getKeyString()));
		final DesugarResult<CoreExpr> projectionDs = methodNameDs.projection(sourceExpr, targetSourceExpr, projectionSourceExpr);
		final DesugarResult<CoreExpr> lazyProjectionDs = projectionDs.function(projectionSourceExpr, projectionDs.getValue());
		final Call lazyAnd = new Call(lookup, new Identifier(Operator.LAZY_AND.getOp()), lazyProjectionDs.getValue());

		return lazyProjectionDs.withDesugared(sourceExpr, lazyAnd);
	}

	/**
	 * <pre> x*.?foo == x.map((x) -> x.?foo)</pre>
	 * <pre> x*.?foo(y) == x.map((x) -> x.?foo(y)</pre>
	 */
	protected DesugarResult<CoreExpr> mapOptionalProjection(BinaryOp op) {
		final DesugarResult<Key> argNameDs = gensym("arg");
		final DesugarResult<CoreExpr> projectionDs = argNameDs.optionalProjection(op, argNameDs.getValue(), op.getRight());
		final DesugarResult<CoreExpr> projectFuncDs = projectionDs.function(op, new MethodParamDecl(argNameDs.getValue()), projectionDs.getValue());
		final DesugarResult<CoreExpr> leftDs = projectFuncDs.expr(op.getLeft());
		final DesugarResult<CoreExpr> mapCallDs = leftDs.withDesugared(op, new Call(leftDs.getValue(), new Identifier("map"), projectFuncDs.getValue()));
		return mapCallDs;
	}

	/**
	 * <pre> x*.foo == x.map((x) -> x.foo)</pre>
	 */
	protected DesugarResult<CoreExpr> mapProjection(BinaryOp op) {
		final DesugarResult<Key> argNameDs = gensym("arg");
		final DesugarResult<CoreExpr> projectionDs = argNameDs.projection(op, (CoreExpr)argNameDs.getValue(), op.getRight());
		final DesugarResult<CoreExpr> projectFuncDs = projectionDs.function(op, new MethodParamDecl(argNameDs.getValue()), projectionDs.getValue());
		final DesugarResult<CoreExpr> leftDs = projectFuncDs.expr(op.getLeft());
		final DesugarResult<CoreExpr> mapCallDs = leftDs.withDesugared(op, new Call(leftDs.getValue(), new Identifier("map"), projectFuncDs.getValue()));
		return mapCallDs;
	}

	/**
	 * Create a function object - an object with a single method with a special name and the given body.
	 */
	protected DesugarResult<CoreExpr> function(SourceExpr op, MethodParamDecl funArg, CoreExpr body) {
		final Method applyMethod = new Method(Method.NO_SELF_NAME, Method.APPLY_FUNCTION_METHOD_NAME, nonNull(Arrays.asList(funArg)), Method.NO_GUARANTEE, body);
		return this.<CoreExpr>withValue(new ObjectLiteral(applyMethod), op);
	}

	/**
	 * Create a function object - an object with a single method with a special name and the given body.
	 */
	protected DesugarResult<CoreExpr> function(SourceExpr op, CoreExpr body) {
		final Method applyMethod = new Method(Method.NO_SELF_NAME, Method.APPLY_FUNCTION_METHOD_NAME, nonNull(Collections.<MethodParamDecl>emptyList()), Method.NO_GUARANTEE, body);
		return this.<CoreExpr>withValue(new ObjectLiteral(applyMethod), op);
	}

	protected DesugarResult<Key> gensym(String s) {
		final Identifier identifier = new Identifier("__g_"+s+this.counter);
		return this.<Key>withValue(identifier, identifier);
	}


	protected DesugarResult<CoreExpr> call(final BinaryOp op) {
		final ArrayList<SourceExpr> argSourceExprs = flattenCommas(op.getRight(), new ArrayList<SourceExpr>());
		return nonNull(op.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
			@Override
			@Nullable
			public DesugarResult<CoreExpr> binaryOp(final BinaryOp calleeOp) {
				// If there's a simple projection with an identifier then it's a direct method call
				if(calleeOp.getOperator() == Operator.PROJECTION) {
					return calleeOp.getRight().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
						@Override
						@Nullable
						public DesugarResult<CoreExpr> key(Key key) {
							return call(op, calleeOp.getLeft(), key, argSourceExprs);
						}

						@Override
						@Nullable
						public DesugarResult<CoreExpr> fallback(SourceExpr other) {
							return callFunction();
						}
					});
				}
				return callFunction();
			}

			@Override
			@Nullable
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				return callFunction();
			}

			public DesugarResult<CoreExpr> callFunction() {
				return call(op, op.getLeft(), Method.APPLY_FUNCTION_METHOD_NAME, argSourceExprs);
			}
		}));
	}

	protected DesugarResult<CoreExpr> call(SourceExpr sourceExpr, SourceExpr object, Key methodName, List<SourceExpr> argSourceExprs) {
		final DesugarResult<CoreExpr> objectDs = expr(object);
		return objectDs.call(sourceExpr, objectDs.getValue(), methodName, argSourceExprs);
	}

	protected DesugarResult<CoreExpr> call(SourceExpr sourceExpr, CoreExpr object, Key methodName, List<SourceExpr> argSourceExprs) {
		final ArrayList<CoreExpr> argCoreExprs = new ArrayList<>(argSourceExprs.size());
		BanjoDesugarer ds = BanjoDesugarer.this;
		for(final SourceExpr argSourceExpr : argSourceExprs) {
			final DesugarResult<CoreExpr> dsr = ds.expr(nonNull(argSourceExpr));
			argCoreExprs.add(dsr.getValue());
			ds = dsr;
		}
		return ds.withDesugared(sourceExpr, new Call(object, methodName, argCoreExprs));
	}

	private DesugarResult<CoreExpr> listLiteral(SourceExpr sourceExpr, SourceExpr elementsExpr, @Nullable Operator requireBullet) {
		final List<SourceExpr> list = flattenCommas(elementsExpr, new ArrayList<SourceExpr>());
		return listLiteral(sourceExpr, list, requireBullet);
	}

	private DesugarResult<CoreExpr> listLiteral(final SourceExpr sourceExpr, List<SourceExpr> list, @Nullable Operator requireBullet) {
		return elements(sourceExpr, list, requireBullet, new F<List<CoreExpr>,CoreExpr>() {
			@Override
			public CoreExpr f(List<CoreExpr> a) {
				return new ListLiteral(a);
			}
		});
	}

	/**
	 * Process a list of expressions, which may make use of the table feature.
	 * 
	 * @param sourceOffset Absolute source offset that the expressions' offsetFromParent is relative to
	 * @param list List of expressions to process
	 * @param requireBullet If true, each expression should be a UnaryOp with this as the operator
	 * @param problems List to add problems to if they are found
	 * @return A list of CoreExpr, each with offsetFromParent relative to the given sourceOffset
	 */
	private DesugarResult<CoreExpr> elements(SourceExpr sourceExpr, final List<SourceExpr> list, @Nullable final Operator requireBullet, F<List<CoreExpr>, CoreExpr> cb) {
		final ArrayList<CoreExpr> elements = new ArrayList<>(list.size());
		BanjoDesugarer currDs = this;
		if(!list.isEmpty()) {
			final ArrayList<SourceExpr> headings = new ArrayList<>();
			for(final SourceExpr e : list) {
				if(e == null) throw new NullPointerException();
				final BanjoDesugarer ds = nonNull(currDs);
				currDs = nonNull(e.acceptVisitor(new BaseSourceExprVisitor<BanjoDesugarer>() {
					@Override
					public BanjoDesugarer unaryOp(UnaryOp op) {
						if(op.getOperator() == Operator.TABLE_HEADER) {
							headings.clear();
							// Table headings here will end up each with an offset relative to the list itself, since they have no parent
							flattenCommas(op.getOperand(), headings);
							return ds;
						} else {
							if(requireBullet != null) {
								if(op.getOperator() == requireBullet) {
									return visitElement(op.getOperand());
								} else {
									elements.add(new BadCoreExpr("Expected "+requireBullet.getOp()));
									return visitElement(op);
								}
							}
							return visitElement(op);
						}
					}

					public BanjoDesugarer visitElement(SourceExpr eltSourceExpr) {
						DesugarResult<CoreExpr> coreEltDs;
						if(!headings.isEmpty()) {
							coreEltDs = ds.makeRow(eltSourceExpr, headings);
						} else {
							coreEltDs = ds.expr(eltSourceExpr);
						}
						elements.add(coreEltDs.getValue());
						return coreEltDs;
					}

					@Override
					public BanjoDesugarer fallback(SourceExpr other) {
						if(requireBullet != null) {
							elements.add(new BadCoreExpr("Expected "+requireBullet.getOp()));
						} else {
						}
						return visitElement(other);
					}
				}));
			}
		}
		return currDs.withDesugared(sourceExpr, nonNull(cb.f(elements)));
	}

	private DesugarResult<CoreExpr> objectLiteral(SourceExpr sourceExpr, SourceExpr methodExprs) {
		final ArrayList<SourceExpr> fieldSourceExprs = flattenCommas(methodExprs, new ArrayList<SourceExpr>());
		return objectLiteral(sourceExpr, fieldSourceExprs);
	}
	private DesugarResult<CoreExpr> objectLiteral(SourceExpr sourceExpr, final Collection<SourceExpr> methodSourceExprs) {
		final ArrayList<SourceExpr> headings = new ArrayList<>();
		DesugarResult<fj.data.List<Method>> ds = this.withValue(nonNull(fj.data.List.<Method>nil()), sourceExpr);
		for(final SourceExpr methodSourceExpr : methodSourceExprs) {
			if(methodSourceExpr == null) throw new NullPointerException();
			ds = ds.addMethod(methodSourceExpr, headings, ds.getValue());
		}
		final fj.data.List<Method> fields = nonNull(ds.getValue().reverse()); // addMethod adds to the start instead of the end
		return ds.withDesugared(sourceExpr, new ObjectLiteral(fields));
	}

	protected DesugarResult<fj.data.List<Method>> addMethod(final SourceExpr fieldSourceExpr, final ArrayList<SourceExpr> headings,
			final fj.data.List<Method> fields) {
		return nonNull(fieldSourceExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<fj.data.List<Method>>>() {
			@Override
			@Nullable
			public DesugarResult<fj.data.List<Method>> binaryOp(BinaryOp op) {
				if(op.getOperator() == Operator.ASSIGNMENT) {
					return visitPair(op);
				} else {
					return fallback(op);
				}
			}

			@Nullable
			private DesugarResult<fj.data.List<Method>> visitPair(BinaryOp fieldOp) {
				final SourceExpr left = fieldOp.getLeft();
				final SourceExpr right = fieldOp.getRight();
				return pair(left, right);
			}

			@Override
			@Nullable
			public DesugarResult<fj.data.List<Method>> unaryOp(UnaryOp op) {
				switch(op.getOperator()) {
				case TABLE_HEADER: return visitTableHeader(op);
				default: return fallback(op);
				}
			}

			@Nullable
			private DesugarResult<fj.data.List<Method>> visitTableHeader(UnaryOp headerOp) {
				headings.clear();
				flattenCommas(headerOp.getOperand(), headings);
				throw new Error("Not implemented");
			}

			@Override
			@Nullable
			public DesugarResult<fj.data.List<Method>> key(Key key) {
				return pair(key, key);
			}

			@Nullable
			private DesugarResult<fj.data.List<Method>> pair(SourceExpr lvalueExpr, SourceExpr valueSourceExpr) {
				final DesugarResult<CoreExpr> eltDs = element(valueSourceExpr, headings);
				return eltDs.addMethod(fieldSourceExpr, lvalueExpr, valueSourceExpr, eltDs.getValue(), Method.NO_GUARANTEE, fields);
			}


			@Override
			@Nullable
			public DesugarResult<fj.data.List<Method>> fallback(SourceExpr other) {
				return addMethod(fieldSourceExpr, new Identifier(other.toSource()), other, new BadCoreExpr("Expected method definition"), Method.NO_GUARANTEE, fields);
			}
		}));
	}

	protected DesugarResult<CoreExpr> element(SourceExpr sourceExpr, ArrayList<SourceExpr> headings) {
		if(headings.isEmpty()) {
			return expr(sourceExpr);
		} else {
			return makeRow(sourceExpr, headings);
		}
	}

	protected DesugarResult<fj.data.List<Method>> addMethod(final SourceExpr methodSourceExpr, final SourceExpr lvalueExpr, final SourceExpr bodySourceExpr, final CoreExpr body, final CoreExpr guarantee, final fj.data.List<Method> methods) {

		return nonNull(lvalueExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<fj.data.List<Method>>>() {
			@Override
			@Nullable
			public DesugarResult<fj.data.List<Method>> binaryOp(BinaryOp targetBOp) {
				switch(targetBOp.getOperator()) {
				case COLON: return methodWithGuarantee(targetBOp);
				case CALL: return methodWithArgs(targetBOp);
				case PROJECTION: return methodWithSelfNameAndNoArgsOrNoName(targetBOp); // self.(x) = ... or self.x = ...
				default: return fallback(targetBOp);
				}
			}

			@Override
			@Nullable
			public DesugarResult<fj.data.List<Method>> unaryOp(final UnaryOp targetOp) {
				switch(targetOp.getOperator()) {
				case PARENS: return targetOp.getOperand().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<fj.data.List<Method>>>() {
					@Override
					@Nullable
					public DesugarResult<fj.data.List<Method>> binaryOp(BinaryOp op) {
						final DesugarResult<Key> selfNameDs = expectIdentifier(op.getLeft());
						final DesugarResult<Method> methodDs = selfNameDs.method(lvalueExpr, bodySourceExpr, new Identifier(op.getOperator().getOp()), op.getRight(), selfNameDs.getValue(), guarantee, body);
						return methodDs.withValue(nonNull(methods.cons(methodDs.getValue())), methodSourceExpr);
					}

					@Override
					@Nullable
					public DesugarResult<fj.data.List<Method>> unaryOp(UnaryOp op) {
						switch(op.getOperator()) {
						case OBJECT_LITERAL:
						case BRACKETS:
							return fallback(op);
						}
						final DesugarResult<Key> selfNameDs = expectIdentifier(op.getOperand());
						final DesugarResult<Method> methodDs = selfNameDs.method(lvalueExpr, bodySourceExpr, new Identifier(op.getOperator().getOp()), nonNull(Collections.<SourceExpr>emptyList()), selfNameDs.getValue(), guarantee, body);
						return methodDs.withValue(nonNull(methods.cons(methodDs.getValue())), methodSourceExpr);
					}
					@Override
					@Nullable
					public DesugarResult<fj.data.List<Method>> fallback(SourceExpr other) {
						return parenMethod(targetOp, Method.APPLY_FUNCTION_METHOD_NAME);
					}
				});
				case BRACKETS: return parenMethod(targetOp, Method.LOOKUP_METHOD_NAME);
				default: return super.unaryOp(targetOp);
				}

			}

			private DesugarResult<fj.data.List<Method>> parenMethod(final UnaryOp signature, Key methodName) {
				final DesugarResult<Method> methodDs = method(lvalueExpr, bodySourceExpr, methodName, signature.getOperand(), Method.NO_SELF_NAME, guarantee, body);
				return methodDs.withValue(nonNull(methods.cons(methodDs.getValue())), methodSourceExpr);
			}

			/**
			 * Desugar a method in one of the two forms:
			 * 
			 *   self.x = ... no-arg method uses self ...
			 *   self.(x) = ... no-name method uses self ...
			 */
			private DesugarResult<fj.data.List<Method>> methodWithSelfNameAndNoArgsOrNoName(final BinaryOp signature) {
				final DesugarResult<Key> selfNameDs = expectIdentifier(signature.getLeft());
				return nonNull(signature.getRight().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<fj.data.List<Method>>>() {

					@Override
					@Nullable
					public DesugarResult<fj.data.List<Method>> unaryOp(UnaryOp signature2) {
						switch(signature2.getOperator()) {
						case PARENS: return specialMethod(signature2, Method.APPLY_FUNCTION_METHOD_NAME);
						case BRACKETS: return specialMethod(signature2, Method.LOOKUP_METHOD_NAME);
						default: return fallback(signature2);
						}
					}

					private DesugarResult<fj.data.List<Method>> specialMethod(UnaryOp signature2, final Key methodName) {
						final DesugarResult<Method> methodDs = selfNameDs.method(lvalueExpr, bodySourceExpr, methodName, signature2.getOperand(), selfNameDs.getValue(), guarantee, body);
						return methodDs.withValue(nonNull(methods.cons(methodDs.getValue())), methodSourceExpr);
					}

					@Override
					@Nullable
					public DesugarResult<fj.data.List<Method>> fallback(SourceExpr signature2) {
						// self.x = ... means self.x() = ...
						final DesugarResult<Key> nameDs = selfNameDs.expectIdentifier(signature2);
						final DesugarResult<Method> methodDs = nameDs.method(lvalueExpr, bodySourceExpr, nameDs.getValue(), new EmptyExpr(), selfNameDs.getValue(), guarantee, body);
						return methodDs.withValue(nonNull(methods.cons(methodDs.getValue())), methodSourceExpr);
					}
				}));
			}

			/**
			 * Desugar the field definition as a method that takes parameters.
			 */
			private DesugarResult<fj.data.List<Method>> methodWithArgs(final BinaryOp signature) {
				final SourceExpr methodLeftExpr = signature.getLeft();
				final SourceExpr argsExpr = signature.getRight();
				return nonNull(methodLeftExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<fj.data.List<Method>>>() {
					@Override
					public DesugarResult<fj.data.List<Method>> binaryOp(BinaryOp methodDefBOp) {
						switch(methodDefBOp.getOperator()) {
						case PROJECTION: return projection(methodDefBOp);
						default: return fallback(methodDefBOp);
						}
					}

					private DesugarResult<fj.data.List<Method>> projection(BinaryOp methodDefBOp) {
						final SourceExpr nameSourceExpr = methodDefBOp.getRight();
						final SourceExpr selfNameSourceExpr = methodDefBOp.getLeft();
						final DesugarResult<Key> selfNameDs = expectIdentifier(selfNameSourceExpr);
						return apply(nameSourceExpr, selfNameDs.getValue(), selfNameDs);
					}

					DesugarResult<fj.data.List<Method>> apply(SourceExpr nameExpr, Key selfName, BanjoDesugarer ds) {
						final DesugarResult<Key> keyDs = ds.expectIdentifier(nameExpr);
						final DesugarResult<Method> methodDs = keyDs.method(
								signature,
								bodySourceExpr,
								keyDs.getValue(),
								argsExpr,
								selfName,
								guarantee,
								body);
						return methodDs.withValue(nonNull(methods.cons(methodDs.getValue())), methodSourceExpr);
					}
					@Override
					public DesugarResult<fj.data.List<Method>> fallback(SourceExpr other) {
						return apply(methodLeftExpr, Method.NO_SELF_NAME, BanjoDesugarer.this);
					}
				}));
			}

			/**
			 * Desugar the field as a method that doesn't take any parameters
			 */
			private DesugarResult<fj.data.List<Method>> methodWithGuarantee(BinaryOp targetBOp) {
				final SourceExpr newLvalueExpr = targetBOp.getLeft();
				final DesugarResult<CoreExpr> newGuaranteeDs = expr(targetBOp.getRight());
				final CoreExpr combinedGuarantee = composeGuarantees(guarantee, newGuaranteeDs.getValue());
				return newGuaranteeDs.addMethod(methodSourceExpr, newLvalueExpr, bodySourceExpr, body, combinedGuarantee, methods);
			}

			/**
			 * In this case the lvalue wasn't something we recognize normally so it should be an identifier, which is the name of the
			 * method.
			 */
			@Override
			public DesugarResult<fj.data.List<Method>> fallback(SourceExpr other) {
				final List<MethodParamDecl> funArgs = nonNull(Collections.<MethodParamDecl>emptyList());
				final DesugarResult<Key> keyDs = expectIdentifier(lvalueExpr);
				final Method method = new Method(Method.NO_SELF_NAME, keyDs.getValue(), funArgs, guarantee, body);
				return keyDs.withValue(nonNull(methods.cons(method)), methodSourceExpr);
			}
		}));

	}

	/**
	 * Desugar something the MUST be an identifier.  If it's not an identifier, a placeholder
	 * is returned - an instance of BadIdentifier - that can be used as an identifier to continue
	 * with desugaring but which will be reported as an error.
	 */
	protected DesugarResult<Key> expectIdentifier(final SourceExpr sourceExpr) {
		return withDesugared(sourceExpr, nonNull(sourceExpr.acceptVisitor(new BaseSourceExprVisitor<Key>() {
			@Override
			public Key key(Key key) {
				return key;
			}

			@Override
			@Nullable
			public Key fallback(SourceExpr other) {
				return new BadIdentifier(sourceExpr);
			}
		})));
	}

	/**
	 * Create an expression combining two guarantees.
	 * 
	 * The resulting expression with have offsetInParent relative to the newGuarantee.
	 * 
	 * @param guarantee Previous guarantee
	 * @param guaranteeSourceOffset Absolute source offset of the guarantee
	 * @param newGuarantee New guarantee to add onto it
	 * @param newGuaranteeSourceOffset Absolute source offset of the new guarantee
	 * @return A new CoreExpr repreesnting the combined guarantee
	 */
	protected CoreExpr composeGuarantees(CoreExpr guarantee, CoreExpr newGuarantee) {
		if(guarantee.equals(Method.NO_GUARANTEE)) {
			return newGuarantee;
		} else if(newGuarantee.equals(Method.NO_GUARANTEE)) {
			return guarantee;
		} else {
			// TODO Implement guarantee merging
			return newGuarantee;
		}
	}

	/**
	 * Make a row from headings
	 * 
	 * @param sourceExpr Source expression for the row
	 * @param sourceOffset Absolute source offset to the expression
	 * @param headings Headings to use to create the record
	 * @param headingsSourceOffset Absolute source offset that the heading's offsetFromParent is relative to
	 * @return
	 */
	private DesugarResult<CoreExpr> makeRow(SourceExpr sourceExpr, List<SourceExpr> headings) {
		final List<SourceExpr> values = flattenCommas(stripParens(sourceExpr), new ArrayList<SourceExpr>(headings.size()));
		for(int i=headings.size(); i < values.size(); i++) {
			final SourceExpr val = values.get(i);
			// TODO Report problems.add(new ExtraTableColumn(i, headings.size(), val.toSource(), valSourceOffset, val.getSourceLength()));
		}
		if(values.size() < headings.size()) {
			// TODO Report problems.add(new MissingValueForTableColumn(values.size(), headings.size(), sourceOffset + sourceExpr.getSourceLength(), 0, headings.get(values.size()).toSource()));
		}
		fj.data.List<Method> fields = ObjectLiteral.EMPTY_METHOD_LIST;
		final int count = Math.min(headings.size(), values.size());
		BanjoDesugarer ds = this;
		for(int i=0; i < count; i++) {
			final SourceExpr headingExpr = nonNull(headings.get(i));
			final SourceExpr cellSourceExpr = nonNull(values.get(i));
			final DesugarResult<CoreExpr> cellCoreExprDs = ds.expr(cellSourceExpr);
			final DesugarResult<fj.data.List<Method>> fieldsDs = cellCoreExprDs.addMethod(cellSourceExpr, headingExpr, cellSourceExpr, cellCoreExprDs.getValue(), Method.NO_GUARANTEE, fields);
			ds = fieldsDs;
			fields = fieldsDs.getValue();
		}
		fields = nonNull(fields.reverse()); // addMethod adds to the start instead of the end
		return ds.withDesugared(sourceExpr, new ObjectLiteral(fields));
	}

	/**
	 * If the expression is wrapped in parentheses, remove them and return a new child expression with an offset reflecting
	 * the offset to the expression inside the parentheses.
	 */
	private SourceExpr stripParens(SourceExpr sourceExpr) {
		return nonNull(sourceExpr.acceptVisitor(new BaseSourceExprVisitor<SourceExpr>() {
			@Override
			@Nullable
			public SourceExpr unaryOp(UnaryOp op) {
				switch(op.getOperator()) {
				case PARENS:
				case RETURN:
					return op.getOperand();
				default:
					return op;
				}
			}

			@Override
			@Nullable
			public SourceExpr fallback(SourceExpr other) {
				return other;
			}
		}));
	}

	/**
	 * Build a function literal.
	 * 
	 * @param sourceExpr Function literal source expression, already identified as a BinaryOp with operator ->
	 * @param sourceOffset Absolute source offset of sourceExpr
	 */
	protected DesugarResult<CoreExpr> functionLiteral(BinaryOp sourceExpr) {
		final DesugarResult<CoreExpr> bodyDs = expr(sourceExpr.getRight());
		final CoreExpr body = bodyDs.getValue();
		final SourceExpr argsSourceExpr = sourceExpr.getLeft();
		return bodyDs.functionLiteral(sourceExpr, argsSourceExpr, sourceExpr.getRight(), body);
	}

	protected DesugarResult<CoreExpr> functionLiteral(SourceExpr sourceExpr,
			final SourceExpr argsSourceExpr, SourceExpr bodySourceExpr, final CoreExpr body) {
		final DesugarResult<Method> methodDs = method(argsSourceExpr, bodySourceExpr, Method.APPLY_FUNCTION_METHOD_NAME, argsSourceExpr, Method.NO_SELF_NAME, Method.NO_GUARANTEE, body);
		return methodDs.withDesugared(sourceExpr, new ObjectLiteral(methodDs.getValue()));
	}

	/**
	 * Create a function literal from an args definition (still in source form) and a body (already desugared).
	 * @param args Argument source expression
	 * @param guarantee Function result guarantee.  Use FunctionLiteral.DEFAULT_GUARANTEE if none specified
	 * @param body Function body as a core expression
	 * @param sourceOffset Absolute source offset of sourceExpr
	 * @param argsSourceOffset Absolute offset in characters to the args
	 * @param bodySourceOffset Absolute source offset of the function body expression
	 * @param guaranteeSourceOffset Absolute source offset of the guarantee; use the same offset as sourceOffset if none specified
	 */
	protected DesugarResult<Method> method(final SourceExpr signatureSourceExpr, final SourceExpr bodySourceExpr, final Key methodName, final SourceExpr args, final Key selfName, final CoreExpr guarantee, final CoreExpr body) {
		return nonNull(args.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<Method>>() {
			@Override
			public DesugarResult<Method> binaryOp(BinaryOp op) {
				switch(op.getOperator()) {
				case COLON:
					// (x,y):Guarantee = ...
					return applyGuarantee(op);
				default:
					return fallback(op);
				}
			}

			private DesugarResult<Method> applyGuarantee(BinaryOp op) {
				final SourceExpr newArgs = op.getLeft();
				final SourceExpr newGuaranteeSourceExpr = op.getRight();
				final DesugarResult<CoreExpr> newGuaranteeDs = expr(newGuaranteeSourceExpr);
				final CoreExpr combinedGuarantee = composeGuarantees(guarantee, newGuaranteeDs.getValue());
				return newGuaranteeDs.method(signatureSourceExpr, bodySourceExpr, methodName, newArgs, selfName, combinedGuarantee, body);
			}

			@Override
			public DesugarResult<Method> fallback(SourceExpr other) {
				// Args should be comma-separated
				final List<SourceExpr> exprs = new LinkedList<>();
				// Optional parentheses around formal parameter list
				flattenCommas(stripParens(args), exprs);
				return method(signatureSourceExpr, bodySourceExpr, methodName, exprs, selfName, guarantee, body);
			}
		}));

	}

	protected DesugarResult<Method> method(SourceExpr signatureSourceExpr, SourceExpr bodySourceExpr, Key methodName, List<SourceExpr> argSourceExprs, Key selfName, CoreExpr guarantee, CoreExpr currBody) {
		final List<MethodParamDecl> args = argSourceExprs.isEmpty() ? nonNull(Collections.<MethodParamDecl>emptyList()) : new ArrayList<MethodParamDecl>(argSourceExprs.size());
		DesugarResult<CoreExpr> currBodyDs = withValue(currBody, bodySourceExpr);
		for(final SourceExpr argExpr : argSourceExprs) {
			currBodyDs = methodParamDecl(args, nonNull(argExpr), nonNull(currBodyDs));
		}
		return currBodyDs.withDesugared(signatureSourceExpr, bodySourceExpr, new Method(selfName, methodName, args, guarantee, currBodyDs.getValue()));
	}

	/**
	 * Desugaring a parameter declaration in a method's formal parameter list.
	 * 
	 * @param paramList List of formal parameters; this is mutated to add the parameter to the end
	 * @param argExpr The source expression for the formal argument declaration
	 * @param bodyDs The desugared function body - this might be wrapped in another function while unpacking parameters
	 * @return The result of the desugaring includes the new source mappings and the new method body
	 */
	public DesugarResult<CoreExpr> methodParamDecl(final List<MethodParamDecl> paramList,
			final SourceExpr argExpr, final DesugarResult<CoreExpr> bodyDs) {
		return nonNull(argExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {

			@Override
			@Nullable
			public DesugarResult<CoreExpr> unaryOp(UnaryOp argOp) {
				switch(argOp.getOperator()) {
				case OBJECT_LITERAL: return unpackObject(argOp);
				case BRACKETS: return unpackList(argOp);
				case LAZY: return unpackLazy(argOp);
				default: return fallback(argOp);
				}
			}

			/**
			 * If we find curly braces, we are pulling fields from a record.
			 * 
			 * ({a,b,c=d}) -> body == (_abc) -> ((a,b,d) -> body)(_abc.a,_abc.b,_abc.c)
			 * 
			 * Note how a pair causes a rename and the left side of the pair is what we pull from the record and
			 * the right side of the pair is the resulting name of the parameter.  This is based on the idea that
			 * if you wrote that expression in the scope of the body it would yield something like the original
			 * record.
			 */
			@Nullable
			public DesugarResult<CoreExpr> unpackObject(UnaryOp pattern) {
				final SourceExpr field = pattern.getOperand();
				final DesugarResult<Key> objectTmpNameDs = bodyDs.gensym("obj");
				final Key objectTmpName = objectTmpNameDs.getValue();
				final DesugarResult<fj.data.List<P2<CoreExpr, SourceExpr>>> argsDs = collectFields(objectTmpNameDs, field, objectTmpName , nonNull(fj.data.List.<P2<CoreExpr, SourceExpr>>nil()));
				final List<SourceExpr> tempParams = new ArrayList<SourceExpr>(); // Method signature
				final List<CoreExpr> args = new ArrayList<>(); // Call site
				for(final P2<CoreExpr, SourceExpr> pair : argsDs.getValue().reverse()) {
					tempParams.add(nonNull(pair._2()));
					args.add(pair._1());
				}
				final DesugarResult<Method> tempMethodDs = method(pattern, bodyDs.getSourceExpr(), Method.APPLY_FUNCTION_METHOD_NAME, tempParams, Method.NO_SELF_NAME, Method.NO_GUARANTEE, bodyDs.getValue());
				final ObjectLiteral func = new ObjectLiteral(tempMethodDs.getValue());
				final Call newBody = new Call(func, Method.APPLY_FUNCTION_METHOD_NAME, args);
				final MethodParamDecl paramDecl = new MethodParamDecl(objectTmpName);
				paramList.add(paramDecl);
				return tempMethodDs.withDesugared(argExpr, paramDecl).withValue((CoreExpr)newBody, bodyDs.getSourceExpr());
			}

			public DesugarResult<fj.data.List<P2<CoreExpr, SourceExpr>>> collectFields(final BanjoDesugarer ds, final SourceExpr field, final Key objectName, final fj.data.List<P2<CoreExpr, SourceExpr>> currFields) {
				return nonNull(field.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<fj.data.List<P2<CoreExpr, SourceExpr>>>>() {
					/**
					 * 
					 * @param argSourceExpr For x = y, this would be the expression for x
					 * @param parameterPattern For x = y, this would be the expression for y
					 * @return
					 */
					DesugarResult<fj.data.List<P2<CoreExpr, SourceExpr>>> field(SourceExpr argSourceExpr, final SourceExpr parameterPattern) {
						final DesugarResult<CoreExpr> argDs = ds.projection(field, (CoreExpr)objectName, argSourceExpr);
						return argDs.withValue(nonNull(currFields.cons(P.p(argDs.getValue(), parameterPattern))), field);
					}

					@Override
					public DesugarResult<fj.data.List<P2<CoreExpr, SourceExpr>>> key(Key key) {
						return field(key, key);
					}

					@Override
					public DesugarResult<fj.data.List<P2<CoreExpr, SourceExpr>>> binaryOp(BinaryOp op) {
						switch(op.getOperator()) {
						case ASSIGNMENT: {
							// Field name = variable name
							return field(op.getLeft(), op.getRight());
						}
						case COMMA:
						case NEWLINE:
						case SEMICOLON: {
							final DesugarResult<fj.data.List<P2<CoreExpr, SourceExpr>>> leftDs = collectFields(ds, op.getLeft(), objectName, currFields);
							return collectFields(leftDs, op.getRight(), objectName, leftDs.getValue());
						}

						default:
							return fallback(op);
						}
					}

					@Override
					public DesugarResult<fj.data.List<P2<CoreExpr, SourceExpr>>> fallback(SourceExpr op) {
						// Unexpected whatever-it-is
						return field(op, op);
					}
				}));
			}

			/**
			 * If we find square brackets we are pulling elements from a sequence.
			 * 
			 * ([a,b,c,d]) -> body == (_abcd) -> ((a,b,c,d) -> body)(_abcd[0], _abcd[1], _abcd[2], _abcd[3])
			 */
			@Nullable
			public final DesugarResult<CoreExpr> unpackList(UnaryOp pattern) {
				final DesugarResult<Key> listTmpNameDs = bodyDs.gensym("lst");
				final Key listTmpName = listTmpNameDs.getValue();
				final ArrayList<SourceExpr> elementSourceExprs = flattenCommas(pattern.getOperand(), new ArrayList<SourceExpr>());
				final List<SourceExpr> tempParams = new ArrayList<SourceExpr>();
				final List<CoreExpr> args = new ArrayList<>();
				for(final ListIterator<SourceExpr> it = elementSourceExprs.listIterator(); it.hasNext(); ) {
					final int n = it.nextIndex();
					final SourceExpr eltSourceExpr = nonNull(it.next());
					tempParams.add(eltSourceExpr);
					args.add(new Call(listTmpName, new Identifier(Operator.LOOKUP.getOp()), new NumberLiteral(nonNull(String.valueOf(n)), n)));
				}
				final DesugarResult<Method> tempMethodDs = listTmpNameDs.method(pattern, bodyDs.getSourceExpr(), Method.APPLY_FUNCTION_METHOD_NAME, tempParams, Method.NO_SELF_NAME, Method.NO_GUARANTEE, bodyDs.getValue());
				final ObjectLiteral func = new ObjectLiteral(tempMethodDs.getValue());
				final Call newBody = new Call(func, Method.APPLY_FUNCTION_METHOD_NAME, args);
				final MethodParamDecl paramDecl = new MethodParamDecl(listTmpName);
				paramList.add(paramDecl);
				return tempMethodDs.withDesugared(argExpr, paramDecl).withValue((CoreExpr)newBody, bodyDs.getSourceExpr());
			}

			@Nullable
			public final DesugarResult<CoreExpr> unpackLazy(UnaryOp pattern) {
				final DesugarResult<Key> lazyTmpNameDs = bodyDs.gensym("lazy");
				final List<SourceExpr> tempParams = nonNull(Collections.singletonList(pattern.getOperand()));
				final DesugarResult<Method> tempMethodDs = lazyTmpNameDs.method(pattern, bodyDs.getSourceExpr(), Method.APPLY_FUNCTION_METHOD_NAME, tempParams, Method.NO_SELF_NAME, Method.NO_GUARANTEE, bodyDs.getValue());
				final ObjectLiteral func = new ObjectLiteral(tempMethodDs.getValue());
				final Call newBody = new Call(func, Method.APPLY_FUNCTION_METHOD_NAME, new Call(lazyTmpNameDs.getValue(), Method.APPLY_FUNCTION_METHOD_NAME));
				final MethodParamDecl paramDecl = new MethodParamDecl(lazyTmpNameDs.getValue());
				paramList.add(paramDecl);
				return tempMethodDs.withDesugared(argExpr, paramDecl).withValue((CoreExpr)newBody, bodyDs.getSourceExpr());
			}

			@Override
			public DesugarResult<CoreExpr> binaryOp(final BinaryOp argOp) {
				if(argOp.getOperator() == Operator.COLON) {
					final DesugarResult<Key> nameDs = expectIdentifier(argOp.getLeft());
					final DesugarResult<CoreExpr> assertionDs = nameDs.expr(argOp.getRight());
					final MethodParamDecl paramDecl = new MethodParamDecl(nameDs.getValue(), assertionDs.getValue());
					paramList.add(paramDecl);
					return assertionDs.withDesugared(argExpr, paramDecl).withValue(bodyDs.getValue(), bodyDs.getSourceExpr());
				} else {
					return fallback(argOp);
				}
			}

			@Override
			public DesugarResult<CoreExpr> key(Key key) {
				final MethodParamDecl paramDecl = new MethodParamDecl(key);
				paramList.add(paramDecl);
				return bodyDs.withDesugared(argExpr, paramDecl).withValue(bodyDs.getValue(), bodyDs.getSourceExpr());
			}
			@Override
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				// TODO Report an error
				return key(new BadIdentifier("Invalid argument specification", other.toSource()));
			}
		}));
	}

	private <L extends List<SourceExpr>> L flattenCommas(final SourceExpr arg, final L exprs) {
		arg.acceptVisitor(new BaseSourceExprVisitor<Void>() {
			@Override
			@Nullable
			public Void emptyExpr(EmptyExpr emptyExpr) {
				// Don't add anything for an empty expression
				return null;
			}

			@Override
			@Nullable
			public Void binaryOp(BinaryOp op) {
				switch (op.getOperator()) {
				case COMMA:
				case SEMICOLON:
				case NEWLINE:
					flattenCommas(op.getLeft(), exprs);
					flattenCommas(op.getRight(), exprs);
					break;

				default:
					fallback(op);
					break;
				}
				return null;
			}

			@Override
			@Nullable
			public Void fallback(SourceExpr other) {
				exprs.add(arg);
				return null;
			}
		});
		return exprs;
	}

	protected DesugarResult<CoreExpr> binaryOpToMethodCall(final BinaryOp op, boolean lazyRightOperand) {
		final SourceExpr leftSourceExpr = op.getLeft();
		final Operator operator = op.getOperator();
		final SourceExpr rightSourceExpr = op.getRight();
		return binaryOpToMethodCall(op, leftSourceExpr, operator, rightSourceExpr, lazyRightOperand);
	}

	protected DesugarResult<CoreExpr> binaryOpToMethodCall(SourceExpr op, final SourceExpr leftSourceExpr, final Operator operator,
			final SourceExpr rightSourceExpr, boolean lazyRightOperand) {
		final DesugarResult<CoreExpr> leftDs = expr(leftSourceExpr);
		final DesugarResult<CoreExpr> rightDs = lazyRightOperand ? leftDs.lazyValue(rightSourceExpr, rightSourceExpr) : leftDs.expr(rightSourceExpr);
		return rightDs.withDesugared(op, binaryOpToMethodCall(leftDs.getValue(), operator, rightDs.getValue()));
	}

	protected Call binaryOpToMethodCall(final CoreExpr leftCoreExpr, final Operator operator,	final CoreExpr rightCoreExpr) {
		return new Call(leftCoreExpr, new Identifier(operator.getOp()), rightCoreExpr);
	}


	protected DesugarResult<CoreExpr> lazyValue(SourceExpr sourceExpr, SourceExpr body) {
		final DesugarResult<CoreExpr> bodyDs = expr(body);
		return bodyDs.functionLiteral(sourceExpr, new EmptyExpr(), body, bodyDs.getValue());
	}

	protected DesugarResult<CoreExpr> singletonListLiteral(UnaryOp op) {
		final SourceExpr operandSourceExpr = op.getOperand();
		final DesugarResult<CoreExpr> operandDs = expr(operandSourceExpr);
		return operandDs.withDesugared(op, new ListLiteral(nonNull(Collections.singletonList(operandDs.getValue()))));
	}

	private DesugarResult<CoreExpr> exprPair(final BinaryOp pairOp) {
		final DesugarResult<CoreExpr> bodyDs = expr(pairOp.getRight());
		return bodyDs.exprPair(pairOp, pairOp.getLeft(), pairOp.getRight(), bodyDs.getValue());
	}

	public DesugarResult<CoreExpr> exprPair(final SourceExpr pairOp, final SourceExpr curr, final SourceExpr bodySourceExpr, final CoreExpr body) {
		return nonNull(curr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
			@Override
			public DesugarResult<CoreExpr> binaryOp(BinaryOp leftOp) {
				switch(leftOp.getOperator()) {
				case MONAD_EXTRACT: return monadExtract(leftOp);
				case ASSIGNMENT: return let(leftOp);

				default:
					return fallback(leftOp);
				}
			}

			private DesugarResult<CoreExpr> let(final BinaryOp letOp) {
				return nonNull(letOp.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
					@Override
					public DesugarResult<CoreExpr> binaryOp(BinaryOp op) {
						switch(op.getOperator()) {
						case CALL: return functionDef(op);
						default:
							return fallback(op);
						}
					}

					public DesugarResult<CoreExpr> functionDef(BinaryOp callOp) {
						// f(x) = y ; z == (f -> z)(x -> y)
						final DesugarResult<Key> funcNameDs = expectIdentifier(callOp.getLeft()); // f
						final DesugarResult<CoreExpr> contDs = funcNameDs.functionLiteral(pairOp, funcNameDs.getValue(), bodySourceExpr, body); // (f -> z)
						final DesugarResult<CoreExpr> rhsDs = contDs.expr(letOp.getRight()); // y
						final DesugarResult<CoreExpr> funcDs = rhsDs.functionLiteral(pairOp, callOp.getRight(), rhsDs.getSourceExpr(), rhsDs.getValue()); // (x -> y)
						return funcDs.withDesugared(pairOp, new Call(contDs.getValue(), Method.APPLY_FUNCTION_METHOD_NAME, funcDs.getValue())); // (f -> z)(x -> y)

					}

					@Override
					public DesugarResult<CoreExpr> fallback(SourceExpr argPattern) {
						// x = y, z == (x -> z)(y) == {(x) = z}(y)
						final DesugarResult<CoreExpr> contDs = functionLiteral(pairOp, argPattern, pairOp, body); // {(x) = z}
						final DesugarResult<CoreExpr> rhsDs = contDs.expr(letOp.getRight()); // y
						return rhsDs.withDesugared(pairOp, new Call(contDs.getValue(), Method.APPLY_FUNCTION_METHOD_NAME, rhsDs.getValue()));
					}
				}));
			}
			private DesugarResult<CoreExpr> monadExtract(BinaryOp arrowOp) {
				// x <- y, z == (y)."<-"(x -> z)
				final DesugarResult<CoreExpr> rhsDs = expr(arrowOp.getRight());
				final DesugarResult<CoreExpr> contDs = rhsDs.functionLiteral(arrowOp, arrowOp.getLeft(), bodySourceExpr, body);
				return contDs.withDesugared(pairOp, new Call(rhsDs.getValue(), new Identifier(Operator.MONAD_EXTRACT.getOp()), contDs.getValue()));
			}

			/**
			 * In this case the left-hand expression isn't a kind of let so we don't have to introduce a new variable.  This
			 * is the "and then" monad operation, denoted ">>" in Haskell but we'll use ";" since ">>" is so bit-shifty.
			 * We have to make the next step lazy, too, to allow the monad to do control flow.
			 * 
			 * x ; y == (x).";"(-> y)
			 */
			@Override
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				final DesugarResult<CoreExpr> rhsDs = expr(other);
				final DesugarResult<CoreExpr> contDs = rhsDs.functionLiteral(other, new EmptyExpr(), bodySourceExpr, body);
				return contDs.withDesugared(pairOp, new Call(rhsDs.getValue(), new Identifier(Operator.SEMICOLON.getOp()), contDs.getValue()));
			}

		}));
	}

	/** When a let is not followed by an expression that uses it, it's kind of an error */
	protected DesugarResult<CoreExpr> trailingLet(BinaryOp op) {
		return exprPair(op, op, op, ObjectLiteral.EMPTY);
	}
	protected DesugarResult<CoreExpr> listLiteral(SourceExpr sourceExpr, final SourceExpr elementsExpr) {
		final LinkedList<SourceExpr> exprs = flattenCommas(elementsExpr, new LinkedList<SourceExpr>());
		return listLiteral(elementsExpr, exprs, null);
	}

	private DesugarResult<CoreExpr> parens(SourceExpr sourceExpr, SourceExpr body) {
		final DesugarResult<CoreExpr> bodyDs = expr(body);
		return bodyDs.withDesugared(sourceExpr, bodyDs.getValue());
	}

	class DesugarVisitor implements SourceExprVisitor<DesugarResult<CoreExpr>> {
		@Override
		public DesugarResult<CoreExpr> binaryOp(final BinaryOp op) {
			// Comma outside of a parentheses should be a list or map without the braces/brackets
			switch(op.getOperator()) {
			case COMMA:
			case SEMICOLON:
			case NEWLINE: {
				return exprPair(op);
			}
			case CALL: return call(op);
			case FUNCTION: return functionLiteral(op);
			case ASSIGNMENT: return trailingLet(op);
			case PROJECTION: return projection(op);
			case OPT_PROJECTION: return optionalProjection(op);
			case MAP_PROJECTION: return mapProjection(op);
			case MAP_OPT_PROJECTION: return mapOptionalProjection(op);


			case GT:
			case GE:
			case LT:
			case LE:
				return comparison(op);


				// Normal operators are translated into a method call
			case EQ: // TODO Maybe use <=> if == isn't defined?
			case POW:
			case MUL:
			case DIV:
			case ADD:
			case SUB:
			case INTERSECT:
			case XOR:
			case UNION:
			case LOOKUP: return binaryOpToMethodCall(op, false);

			case EXTEND: return extend(op);

			// Short-circuit operators have a lazy right operand
			case LAZY_AND:
			case LAZY_OR:
			case COND: return binaryOpToMethodCall(op, true);

			default:
				return withDesugared(op, new BadCoreExpr("Unimplemented binary operator: '"+op.getOperator()+"'"));
			}
		}

		@Override
		public DesugarResult<CoreExpr> unaryOp(final UnaryOp op) {
			final SourceExpr operandSourceExpr = op.getOperand();
			switch(op.getOperator()) {
			case LIST_ELEMENT: return singletonListLiteral(op);
			case LAZY: return lazyValue(op, op.getOperand());
			case BRACKETS: return listLiteral(op, operandSourceExpr);
			case OBJECT_LITERAL: return objectLiteral(op, operandSourceExpr);

			case PARENS:
			case RETURN:
			case UNARY_NEWLINE_INDENT:
				return parens(op, operandSourceExpr);

			case OPTIONAL:
			case EXISTS:
			case NOT:
			case COMPLEMENT:
			case PLUS:
			case NEGATE:
				final String methodName = op.getOperator().getOp();
				final DesugarResult<CoreExpr> operandCoreExpr = expr(operandSourceExpr);
				return operandCoreExpr.withDesugared(op, new Call(operandCoreExpr.getValue(), new Identifier(methodName)));

			case INSPECT:
				return inspect(op);

			case INVALID:
				return withDesugared(op, new BadCoreExpr("Invalid unary operator"));
			default:
				return withDesugared(op, new BadCoreExpr("Unimplemented unary operator: '"+op.getOperator()+"'"));

			}
		}

		@Override
		public DesugarResult<CoreExpr> stringLiteral(StringLiteral stringLiteral) {
			return withDesugared(stringLiteral, (CoreExpr)stringLiteral);
		}

		@Override
		public DesugarResult<CoreExpr> numberLiteral(NumberLiteral numberLiteral) {
			return withDesugared(numberLiteral, numberLiteral);
		}

		@Override
		public DesugarResult<CoreExpr> identifier(Identifier simpleName) {
			return withDesugared(simpleName, (CoreExpr)simpleName);
		}

		@Override
		public DesugarResult<CoreExpr> operator(OperatorRef operatorRef) {
			return withDesugared(operatorRef, operatorRef);
		}

		//	@Override @Nullable
		//	public CoreExpr visitWhitespace(Whitespace ws) {
		//		return null;
		//	}
		//
		//	@Override @Nullable
		//	public CoreExpr visitComment(Comment c) {
		//		return null;
		//	}
		//
		//	@Override @Nullable
		//	public CoreExpr visitEof() {
		//		return null;
		//	}

		@Override
		public DesugarResult<CoreExpr> ellipsis(Ellipsis ellipsis) {
			return withDesugared(ellipsis, new BadCoreExpr("Unexpected ellipsis"));
		}
		@Override
		@Nullable
		public DesugarResult<CoreExpr> badSourceExpr(BadSourceExpr badSourceExpr) {
			return withDesugared(badSourceExpr, new BadCoreExpr(badSourceExpr.getMessage()));
		}
		@Override
		@Nullable
		public DesugarResult<CoreExpr> emptyExpr(EmptyExpr emptyExpr) {
			return withDesugared(emptyExpr, new BadCoreExpr("Expected expression"));
		}
	}

	public DesugarMap getDesugarMap() {
		return this.desugarMap;
	}

	public DesugarResult<CoreExpr> inspect(UnaryOp op) {
		final DesugarResult<CoreExpr> exprDs = expr(op.getOperand());
		return exprDs.withDesugared(op, new Inspect(exprDs.getValue()));
	}

	public DesugarResult<CoreExpr> extend(BinaryOp op) {
		final DesugarResult<CoreExpr> leftDs = expr(op.getLeft());
		final DesugarResult<CoreExpr> rightDs = leftDs.expr(op.getRight());
		return rightDs.withDesugared(op, new Extend(leftDs.getValue(), rightDs.getValue()));
	}


}
