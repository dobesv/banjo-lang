package banjo.desugar;

import static banjo.parser.util.Check.nonNull;
import static fj.data.List.cons;
import static fj.data.List.single;
import banjo.dom.BadExpr;
import banjo.dom.core.BadCoreExpr;
import banjo.dom.core.Call;
import banjo.dom.core.CoreErrorGatherer;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.Extend;
import banjo.dom.core.Inspect;
import banjo.dom.core.Let;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.MixfixFunctionIdentifier;
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
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.parser.util.SourceFileRange;
import fj.F;
import fj.F2;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Set;
import fj.data.TreeMap;

public class SourceExprDesugarer {
	protected static final List<SourceFileRange> NOT_FROM_SOURCE = List.nil();
	static final Set<String> EMPTY_STRING_SET = Set.empty(Ord.stringOrd);

	public static class DesugarResult<T> extends SourceExprDesugarer {
		final SourceExpr sourceExpr;
		final T value;
		public DesugarResult(T expr, SourceExpr sourceExpr) {
			this.value = expr;
			this.sourceExpr = sourceExpr;
		}

		public T getValue() {
			return this.value;
		}

		public SourceExpr getSourceExpr() {
			return this.sourceExpr;
		}

		public List<BadExpr> getProblems() {
			return ((CoreExpr) this.value).acceptVisitor(new CoreErrorGatherer());
		}


		<TT> DesugarResult<TT> mapValue(F2<SourceExprDesugarer,T,TT> f) {
			return withValue(f.f(this, getValue()));
		}

	}
	public static final TreeMap<SourceFileRange, Set<BadExpr>> EMPTY_ERROR_MAP = TreeMap.empty(SourceFileRange.ORD);
	static final fj.data.Set<SourceExpr> EMPTY_SOURCE_EXPR_SET = fj.data.Set.empty(SourceExpr.ORD);

	<TT> DesugarResult<TT> withValue(TT result) {
		return withValue(result, null);
	}

	<TT> DesugarResult<TT> withValue(TT result, SourceExpr sourceExpr) {
		return new DesugarResult<TT>(result, sourceExpr);
	}

	protected DesugarResult<CoreExpr> withDesugared(SourceExpr sourceExpr, CoreExpr expr) {
		return new DesugarResult<CoreExpr>(expr, sourceExpr);
	}

	protected DesugarResult<Key> withDesugared(SourceExpr sourceExpr, Key expr) {
		return new DesugarResult<Key>(expr, sourceExpr);
	}

	protected DesugarResult<Key> withIdentifier(Key id) {
		return new DesugarResult<Key>(id, null);
	}

	protected DesugarResult<FunctionLiteral> withDesugared(FunctionLiteral method, SourceExpr methodSourceExpr) {
		return new DesugarResult<FunctionLiteral>(method, methodSourceExpr);
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
		return nonNull(sourceExpr.acceptVisitor(new DesugarVisitor()));
	}

	/**
	 * Desugar a few exprs at once.  The state is accumulated from one to the next so that the
	 * last element of the array has the accumulated cache from the previous desugarings.
	 */
	public DesugarResult<CoreExpr[]> exprs(SourceExpr parent, SourceExpr ... sourceExprs) {
		final CoreExpr[] results = new CoreExpr[sourceExprs.length];
		SourceExprDesugarer desugarer = this;
		for(int i=0; i < sourceExprs.length; i++) {

			final DesugarResult<CoreExpr> resultDs = desugarer.expr(sourceExprs[i]);
			desugarer = resultDs;
			results[i] = resultDs.getValue();
		}
		return new DesugarResult<CoreExpr[]>(results, parent);
	}

	/**
	 * Projection.
	 *
	 * @see Projection
	 * @param op BinaryOp describing the projection (having PROJECTION as its operator)
	 * @param callNext TODO
	 * @param optional TODO
	 * @param sourceOffset Source offset to the start of op
	 */
	protected DesugarResult<CoreExpr> projection(BinaryOp op, boolean callNext, boolean optional) {
		return projection(op, op.getLeft(), op.getRight(), callNext, optional);
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
	 * @param objectExpr Left-hand side of the projection; the "base" object we're projecting from
	 * @param projectionExpr Right-hand side of the projection
	 * @param callNext TODO
	 * @param optional TODO
	 */
	protected DesugarResult<CoreExpr> projection(final SourceExpr sourceExpr, final SourceExpr objectExpr, final SourceExpr projectionExpr, boolean callNext, boolean optional) {
		final DesugarResult<CoreExpr> objectDs = expr(objectExpr);
		return objectDs.projection(sourceExpr, objectDs.getValue(), projectionExpr, callNext, optional);
	}

	protected DesugarResult<CoreExpr> projection(final SourceExpr sourceExpr, final CoreExpr objectCoreExpr, final SourceExpr projectionExpr, final boolean callNext, final boolean optional) {
		return nonNull(projectionExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
			@Override
			public DesugarResult<CoreExpr> key(Key key) {
				return withDesugared(sourceExpr, new Call(sourceExpr.getSourceFileRanges(), objectCoreExpr, key, List.nil(), callNext, optional));
			}

			@Override
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				return withDesugared(sourceExpr, new BadCoreExpr(sourceExpr.getSourceFileRanges(), "Expected identifier after '.'"));
			}

		}));
	}

	/**
	 * <pre> x*.foo == x.map((x) -> x.foo)</pre>
	 * @param callNext TODO
	 * @param optional TODO
	 */
	protected DesugarResult<CoreExpr> mapProjection(BinaryOp op, boolean callNext, boolean optional) {
		final DesugarResult<CoreExpr> leftDs = expr(op.getLeft());
		CoreExpr target = leftDs.getValue();
		SourceExpr projection = op.getRight();
		return leftDs.mapProjection(op, target, projection, callNext, optional);
	}

	protected DesugarResult<CoreExpr> mapProjection(SourceExpr op, CoreExpr target,	SourceExpr projection, boolean callNext, boolean optional) {
		final Key argName = new Identifier("//compiler/map projection/x");
		final DesugarResult<CoreExpr> projectionDs = projection(op, (CoreExpr)argName, projection, callNext, optional);
		final DesugarResult<CoreExpr> projectFuncDs = projectionDs.function(op, argName, projectionDs.getValue());
		final DesugarResult<CoreExpr> mapCallDs = projectFuncDs.withDesugared(op, new Call(NOT_FROM_SOURCE, target, new Identifier("map"), projectFuncDs.getValue()));
		return mapCallDs;
	}

	/**
	 * Create a function object - an object with a single method with a special name and the given body.
	 */
	protected DesugarResult<CoreExpr> function(SourceExpr op, Key funArg, CoreExpr body) {
		final FunctionLiteral applyMethod = FunctionLiteral.function(funArg, body);
		return this.<CoreExpr>withValue(new ObjectLiteral(applyMethod));
	}

	protected DesugarResult<CoreExpr> call(final BinaryOp op) {
		return call(op, Key.ANONYMOUS, List.nil());
	}

	protected DesugarResult<CoreExpr> pipeTo(final BinaryOp op) {
		DesugarResult<CoreExpr> leftDs = desugar(op.getLeft());
		DesugarResult<CoreExpr> rightDs = leftDs.desugar(op.getRight());
		return withDesugared(op, new Call(op.getSourceFileRanges(), rightDs.getValue(), Key.ANONYMOUS, leftDs.getValue()));
	}

	protected DesugarResult<CoreExpr> pipeFrom(final BinaryOp op) {
		DesugarResult<CoreExpr> leftDs = desugar(op.getLeft());
		DesugarResult<CoreExpr> rightDs = leftDs.desugar(op.getRight());
		return withDesugared(op, new Call(op.getSourceFileRanges(), leftDs.getValue(), Key.ANONYMOUS, rightDs.getValue()));
	}

	protected Key concatNameParts(Key prefix, Key suffix) {
		List<SourceFileRange> ranges = suffix.getSourceFileRanges().append(prefix.getSourceFileRanges());
		final List<String> suffixParts = suffix.getParts();
		if(suffixParts.isEmpty())
			return prefix;
		final List<String> prefixParts = prefix.getParts();
		if(prefixParts.isEmpty())
			return suffix;
		List<String> nameParts = prefixParts.append(suffixParts);
		return new MixfixFunctionIdentifier(ranges, nameParts);
	}

	protected DesugarResult<CoreExpr> call(final BinaryOp op, final Key moreNames, final List<List<SourceExpr>> moreArgumentLists) {
		return call(op, moreNames, moreArgumentLists, false, false);
	}

	protected DesugarResult<CoreExpr> call(final BinaryOp op, final Key moreNames, final List<List<SourceExpr>> moreArgumentLists, final boolean callNext, final boolean optional) {
		final List<SourceExpr> argSourceExprs = flattenCommas(op.getRight());
		return nonNull(op.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
			@Override
			public DesugarResult<CoreExpr> binaryOp(final BinaryOp calleeOp) {
				// If there's a simple projection with an identifier then it's a direct method call
				switch(calleeOp.getOperator()) {
				case PROJECTION:
				case OPT_PROJECTION:
				case CALL_NEXT_METHOD:
				case OPT_CALL_NEXT_METHOD:
					return calleeOp.getRight().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
						@Override
						public DesugarResult<CoreExpr> key(Key key) {
							boolean actualCallNext = callNext || calleeOp.getOperator() == Operator.CALL_NEXT_METHOD || calleeOp.getOperator() == Operator.OPT_CALL_NEXT_METHOD;
							boolean actualOptional = optional || calleeOp.getOperator() == Operator.OPT_PROJECTION || calleeOp.getOperator() == Operator.OPT_CALL_NEXT_METHOD;
							return call(op, calleeOp.getLeft(), concatNameParts(key, moreNames), cons(argSourceExprs, moreArgumentLists), actualCallNext, actualOptional);
						}

						@Override
						public DesugarResult<CoreExpr> fallback(SourceExpr other) {
							return callFunction();
						}
					});
				case MAP_PROJECTION:
				case MAP_OPT_PROJECTION:
					return calleeOp.getRight().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
						@Override
						public DesugarResult<CoreExpr> key(Key key) {
							return callMethod(op, moreNames, moreArgumentLists,
									optional, argSourceExprs, calleeOp, key);
						}

						@Override
						public DesugarResult<CoreExpr> fallback(SourceExpr other) {
							return callFunction();
						}
					});

				case JUXTAPOSITION:
					return calleeOp.getRight().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
						public SourceExprDesugarer.DesugarResult<CoreExpr> key(final Key key) {
							return nonNull(calleeOp.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
								public SourceExprDesugarer.DesugarResult<CoreExpr> binaryOp(BinaryOp calleeLeftOp) {
									if(calleeLeftOp.getOperator() == Operator.CALL) {
										return SourceExprDesugarer.this.call(
												calleeLeftOp,
												concatNameParts(key, moreNames),
												cons(argSourceExprs, moreArgumentLists),
												callNext,
												optional
										);
									} else {
										return callFunction();
									}
								}

								public SourceExprDesugarer.DesugarResult<CoreExpr> fallback(SourceExpr other) {
									return callFunction();
								}
							}));
						}

						public SourceExprDesugarer.DesugarResult<CoreExpr> fallback(SourceExpr other) {
							return callFunction();
						}
					});
				default:
					return callFunction();
				}
			}

			// Calling a function by name - be sure to put all the name parts together if there are
			// more than.
			@Override
			public DesugarResult<CoreExpr> key(Key key) {
				return call(
						op,
						(CoreExpr)concatNameParts(key, moreNames),
						Key.ANONYMOUS,
						cons(argSourceExprs, moreArgumentLists),
						callNext,
						optional);
			}

			@Override
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				return callFunction();
			}

			public DesugarResult<CoreExpr> callFunction() {
				return call(op, op.getLeft(), moreNames, cons(argSourceExprs, moreArgumentLists), callNext, optional);
			}
		}));
	}

	protected DesugarResult<CoreExpr> call(SourceExpr sourceExpr, SourceExpr object, final Key name, final List<List<SourceExpr>> argumentLists, boolean callNext, boolean optional) {
		final DesugarResult<CoreExpr> objectDs = expr(object);
		return objectDs.call(sourceExpr, objectDs.getValue(), name, argumentLists, callNext, optional);
	}

	protected DesugarResult<CoreExpr> call(SourceExpr sourceExpr, CoreExpr object, final Key name, final List<List<SourceExpr>> argumentLists, boolean callNext, boolean optional) {
		DesugarResult<List<List<CoreExpr>>> dsParts = desugarArgLists(argumentLists);
		return dsParts.withDesugared(sourceExpr, new Call(sourceExpr.getSourceFileRanges(), object, name, dsParts.getValue(), callNext, optional));
	}

	private DesugarResult<List<List<CoreExpr>>> desugarArgLists(
            final List<List<SourceExpr>> argumentLists) {
	    DesugarResult<List<List<CoreExpr>>> dsParts = argumentLists.foldRight(new F2<List<SourceExpr>, DesugarResult<List<List<CoreExpr>>>, DesugarResult<List<List<CoreExpr>>>>() {
			@Override
			public DesugarResult<List<List<CoreExpr>>> f(List<SourceExpr> argSourceExprs, DesugarResult<List<List<CoreExpr>>> b) {
				return b.desugarArgList(argSourceExprs, b.getValue());
			}
		}, withValue(List.nil()));
	    return dsParts;
    }

	private DesugarResult<CoreExpr> listLiteral(final SourceExpr sourceExpr, List<SourceExpr> list, Operator requireBullet) {
		return elements(sourceExpr, list, requireBullet, new F<DesugarResult<List<CoreExpr>>,DesugarResult<CoreExpr>>() {
			@Override
			public DesugarResult<CoreExpr> f(DesugarResult<List<CoreExpr>> ds) {
				return nonNull(ds).withDesugared(sourceExpr, new ListLiteral(sourceExpr.getSourceFileRanges(), nonNull(ds).getValue()));
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
	private DesugarResult<CoreExpr> elements(SourceExpr sourceExpr, final List<SourceExpr> list, final Operator requireBullet, F<DesugarResult<List<CoreExpr>>, DesugarResult<CoreExpr>> cb) {

		// First scan for table rows.  We accumulate rows until we find a table header.  If there are rows with no table header they'll fall out of this and get handled next.
		final DesugarResult<P2<List<SourceExpr>, List<CoreExpr>>> rowsDs = list.foldRight(new F2<SourceExpr,DesugarResult<P2<List<SourceExpr>, List<CoreExpr>>>,DesugarResult<P2<List<SourceExpr>, List<CoreExpr>>>>() {

			@Override
			public DesugarResult<P2<List<SourceExpr>, List<CoreExpr>>> f(
					SourceExpr e,
					final DesugarResult<P2<List<SourceExpr>, List<CoreExpr>>> ds) {
				if(ds == null) throw new NullPointerException();
				final List<SourceExpr> unprocessedRows = ds.getValue()._1();
				final List<CoreExpr> processedRows = nonNull(ds.getValue()._2());
				return nonNull(nonNull(e).acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<List<SourceExpr>, List<CoreExpr>>>>() {
					@Override
					public DesugarResult<P2<List<SourceExpr>, List<CoreExpr>>> unaryOp(UnaryOp op) {
						if(op.getOperator() == Operator.TABLE_HEADER) {
							final List<SourceExpr> headings = flattenCommas(op.getOperand());
							final DesugarResult<List<CoreExpr>> rowsDs = unprocessedRows.foldRight(new F2<SourceExpr,DesugarResult<List<CoreExpr>>, DesugarResult<List<CoreExpr>>>() {

								@Override
								public DesugarResult<List<CoreExpr>> f(SourceExpr a, DesugarResult<List<CoreExpr>> ds) {
									if(ds == null) throw new NullPointerException();
									if(a == null) throw new NullPointerException();
									final DesugarResult<CoreExpr> rowDs = ds.makeRow(a, headings);
									return rowDs.withValue(ds.getValue().cons(rowDs.getValue()));
								}

							}, ds.withValue(processedRows));
							return rowsDs.withValue(P.p(List.nil(), rowsDs.getValue()));
						} else {
							return fallback(op);
						}
					}

					@Override
					public DesugarResult<P2<List<SourceExpr>, List<CoreExpr>>> fallback(SourceExpr other) {
						return ds.withValue(P.p(ds.getValue()._1().cons(other), ds.getValue()._2()));
					}
				}));
			}

		}, this.withValue(P.p(List.nil(), List.nil())));

		// Now the remaining "unprocessed rows" are the ones with no table header, so handle those ones next
		final List<SourceExpr> unprocessedElts = rowsDs.getValue()._1();
		final DesugarResult<List<CoreExpr>> eltsDs = unprocessedElts.foldRight(new F2<SourceExpr,DesugarResult<List<CoreExpr>>, DesugarResult<List<CoreExpr>>>() {

			@Override
			public DesugarResult<List<CoreExpr>> f(SourceExpr e,
					final DesugarResult<List<CoreExpr>> ds) {
				if(ds == null) throw new NullPointerException();
				return nonNull(nonNull(e).acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<CoreExpr>>>() {
					@Override
					public DesugarResult<List<CoreExpr>> unaryOp(UnaryOp op) {
						if(requireBullet != null) {
							if(op.getOperator() == requireBullet) {
								return visitElement(op.getOperand());
							} else {
								final DesugarResult<List<CoreExpr>> eltDs = visitElement(op);
								final BadCoreExpr err = new BadCoreExpr(op.getSourceFileRanges(), "Expected "+requireBullet.getOp());
								return eltDs.withDesugared(op, err).withValue(eltDs.getValue().cons(err));
							}
						}
						return visitElement(op);
					}

					public DesugarResult<List<CoreExpr>> visitElement(SourceExpr eltSourceExpr) {
						final DesugarResult<CoreExpr> coreEltDs = ds.expr(eltSourceExpr);
						return coreEltDs.withValue(ds.getValue().cons(coreEltDs.getValue()));
					}

					@Override
					public DesugarResult<List<CoreExpr>> fallback(SourceExpr other) {
						if(requireBullet != null) {
							final BadCoreExpr err = new BadCoreExpr(other.getSourceFileRanges(), "Expected "+requireBullet.getOp());
							return ds.withDesugared(other, err).withValue(ds.getValue().cons(err));
						} else {
						}
						return visitElement(other);
					}
				}));
			}
		}, rowsDs.withValue(nonNull(rowsDs.getValue()._2())));
		return nonNull(cb.f(eltsDs));
	}

	private DesugarResult<CoreExpr> objectLiteral(SourceExpr sourceExpr, SourceExpr methodExprs) {
		final List<SourceExpr> fieldSourceExprs = flattenCommas(methodExprs);
		return objectLiteral(sourceExpr, fieldSourceExprs);
	}
	private DesugarResult<CoreExpr> objectLiteral(SourceExpr sourceExpr, final List<SourceExpr> methodSourceExprs) {
		final DesugarResult<List<FunctionLiteral>> methodsDs = methodSourceExprs.foldRight(new F2<SourceExpr,DesugarResult<List<FunctionLiteral>>,DesugarResult<List<FunctionLiteral>>>() {
			@Override
			public DesugarResult<List<FunctionLiteral>> f(SourceExpr methodSourceExpr, DesugarResult<List<FunctionLiteral>> ds) {
				if(ds == null) throw new NullPointerException();
				final SourceExpr se = nonNull(methodSourceExpr);
				return ds.addMethod(se, List.nil(), ds.getValue());
			}
		}, this.withValue(List.nil()));
		return methodsDs.withDesugared(sourceExpr, new ObjectLiteral(sourceExpr.getSourceFileRanges(), methodsDs.getValue()));
	}

	protected DesugarResult<List<FunctionLiteral>> addMethod(final SourceExpr fieldSourceExpr, final List<SourceExpr> headings, final List<FunctionLiteral> fields) {
		return nonNull(fieldSourceExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<FunctionLiteral>>>() {
			@Override
			public DesugarResult<List<FunctionLiteral>> binaryOp(BinaryOp op) {
				switch(op.getOperator()) {
				case ASSIGNMENT: return visitPair(op, Operator.ASSIGNMENT);
				case EXTEND_METHOD: return visitPair(op, Operator.EXTEND);
				case ADD_METHOD: return visitPair(op, Operator.ADD);
				case SUB_METHOD: return visitPair(op, Operator.SUB);
				case MUL_METHOD: return visitPair(op, Operator.MUL);
				case DIVID_METHOD: return visitPair(op, Operator.DIV);
				case UNION_METHOD: return visitPair(op, Operator.UNION);
				case AND_METHOD: return visitPair(op, Operator.AND_METHOD);
				case OR_METHOD: return visitPair(op, Operator.OR_METHOD);
				default:
					return fallback(op);
				}
			}

			private DesugarResult<List<FunctionLiteral>> visitPair(BinaryOp fieldOp, Operator combiningOp) {
				final SourceExpr left = fieldOp.getLeft();
				final SourceExpr right = fieldOp.getRight();
				return pair(left, right, combiningOp);
			}

			@Override
			public DesugarResult<List<FunctionLiteral>> unaryOp(UnaryOp op) {
				switch(op.getOperator()) {
				case TABLE_HEADER: return visitTableHeader(op);
				default: return fallback(op);
				}
			}

			private DesugarResult<List<FunctionLiteral>> visitTableHeader(UnaryOp headerOp) {
				throw new Error("Headings not supported ...");
			}

			@Override
			public DesugarResult<List<FunctionLiteral>> key(Key key) {
				return pair(key, key, Operator.ASSIGNMENT);
			}

			private DesugarResult<List<FunctionLiteral>> pair(SourceExpr lvalueExpr, SourceExpr valueSourceExpr, Operator combiningOp) {
				final DesugarResult<CoreExpr> eltDs = element(valueSourceExpr, headings);
				// TODO Apply combiningOp operator ...
				return eltDs.addMethod(fieldSourceExpr, lvalueExpr, eltDs.getValue(), FunctionLiteral.EMPTY_POSTCONDITION, fields);
			}


			@Override
			public DesugarResult<List<FunctionLiteral>> fallback(SourceExpr other) {
				return addMethod(fieldSourceExpr, new Identifier(other.getSourceFileRanges(), other.toSource()), new BadCoreExpr(other.getSourceFileRanges(), "Expected method definition"), FunctionLiteral.EMPTY_POSTCONDITION, fields);
			}
		}));
	}

	protected DesugarResult<CoreExpr> element(SourceExpr sourceExpr, List<SourceExpr> headings) {
		if(headings.isEmpty()) {
			return expr(sourceExpr);
		} else {
			return makeRow(sourceExpr, headings);
		}
	}

	Key opMethodName(List<SourceFileRange> operatorRanges, Operator op) {
		if(op == Operator.CALL) return Key.ANONYMOUS;
		return new Identifier(operatorRanges, op.getMethodName());
	}
	Key opMethodName(Operator op) {
		return opMethodName(NOT_FROM_SOURCE, op);
	}
	protected DesugarResult<List<FunctionLiteral>> addMethod(final SourceExpr methodSourceExpr, final SourceExpr signatureSourceExpr, final CoreExpr body, final CoreExpr postcondition, final List<FunctionLiteral> methods) {

		final DesugarResult<List<FunctionLiteral>> result = signatureSourceExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<FunctionLiteral>>>() {
			@Override
			public DesugarResult<List<FunctionLiteral>> binaryOp(BinaryOp targetBOp) {
				switch(targetBOp.getOperator()) {
				case MEMBER_OF: return methodWithGuarantee(targetBOp);
				case CALL: return methodWithArgs(targetBOp);
				case JUXTAPOSITION: return mixfixMethodPart(targetBOp); // a(b)c ... mixfix with no final argument list
				case PROJECTION: return methodWithSelfNameAndNoArgsOrNoName(targetBOp); // self.(x) = ... or self.x = ...
				default: return fallback(targetBOp);
				}
			}


			@Override
			public DesugarResult<List<FunctionLiteral>> unaryOp(final UnaryOp targetOp) {
				switch(targetOp.getOperator()) {
				case PARENS:
					return targetOp.getOperand().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<FunctionLiteral>>>() {
						@Override
						public DesugarResult<List<FunctionLiteral>> binaryOp(BinaryOp op) {
							switch(op.getOperator()) {
							case COMMA:
							case NEWLINE:
							case JUXTAPOSITION:
								return fallback(op);
							default:
							}
							final boolean selfOnRight = op.getOperator().isSelfOnRightMethodOperator();
							final SourceExpr self = selfOnRight ? op.getRight(): op.getLeft();
							final SourceExpr other = selfOnRight ? op.getLeft() : op.getRight();
							final Key selfArg = expectIdentifier(self);
							final DesugarResult<FunctionLiteral> methodDs = method(methodSourceExpr, op.getOperator(), other, selfArg, body);
							return methodDs.withValue(nonNull(methods.cons(methodDs.getValue())));
						}

						@Override
						public DesugarResult<List<FunctionLiteral>> unaryOp(UnaryOp op) {
							switch(op.getOperator()) {
							case OBJECT_LITERAL:
							case BRACKETS:
								return fallback(op);
							default:
							}
							final Key selfArg = expectIdentifier(op.getOperand());
							final DesugarResult<FunctionLiteral> methodDs = method(methodSourceExpr, op.getOperator(), new EmptyExpr(), selfArg, body);
							return methodDs.withValue(nonNull(methods.cons(methodDs.getValue())));
						}
						@Override
						public DesugarResult<List<FunctionLiteral>> fallback(SourceExpr other) {
							return parenMethod(targetOp, Key.ANONYMOUS);
						}
					});
				default: return super.unaryOp(targetOp);
				}

			}

			private DesugarResult<List<FunctionLiteral>> parenMethod(final UnaryOp signature, Key methodName) {
				final DesugarResult<FunctionLiteral> methodDs = method(methodSourceExpr, methodName, signature.getOperand(), Key.ANONYMOUS, body);
				return methodDs.withValue(nonNull(methods.cons(methodDs.getValue())));
			}

			private DesugarResult<List<FunctionLiteral>> methodWithSelfNameAndNoArgsOrNoName(final BinaryOp signature) {
				final Key selfName = expectIdentifier(signature.getLeft());
				return nonNull(signature.getRight().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<FunctionLiteral>>>() {

					@Override
					public DesugarResult<List<FunctionLiteral>> unaryOp(UnaryOp signature2) {
						switch(signature2.getOperator()) {
						case PARENS: return specialMethod(signature2, Key.ANONYMOUS);
						default: return fallback(signature2);
						}
					}

					private DesugarResult<List<FunctionLiteral>> specialMethod(UnaryOp signature2, final Key methodName) {
						final DesugarResult<FunctionLiteral> methodDs = method(methodSourceExpr, methodName, signature2.getOperand(), selfName, body);
						return methodDs.withValue(nonNull(methods.cons(methodDs.getValue())));
					}

					@Override
					public DesugarResult<List<FunctionLiteral>> fallback(SourceExpr signature2) {
						// self.x = ... means self.x() = ...
						final Key name = expectIdentifier(signature2);
						final DesugarResult<FunctionLiteral> methodDs = method(methodSourceExpr, name, List.nil(), selfName, body);
						return methodDs.withValue(nonNull(methods.cons(methodDs.getValue())));
					}
				}));
			}

			/**
			 * Desugar a method signature with a trailing word after the argument list(s).
			 *
			 * The juxtaposition should split a signature like this:
			 *
			 * ex: a(b) c
			 *
			 * <pre>
			 * a  (b)
			 * \  /
			 * CALL   c
			 *   \    /
			 *  JUXTAPOSITION
			 * </pre>
			 */
			private DesugarResult<List<FunctionLiteral>> mixfixMethodPart(final BinaryOp juxtaposition) {
				return juxtaposition.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<FunctionLiteral>>>() {
					@Override
					public DesugarResult<List<FunctionLiteral>> fallback(SourceExpr other) {
						BadCoreExpr problem = new BadCoreExpr(juxtaposition.getSourceFileRanges(), "Invalid method signature");
						return withValue(methods.snoc(FunctionLiteral.property(Key.ANONYMOUS, problem)));
					}

					@Override
					public DesugarResult<List<FunctionLiteral>> binaryOp(BinaryOp op) {
						switch(op.getOperator()) {
						case CALL: {
							Key newNameSuffix = expectIdentifier(juxtaposition.getRight());
							List<SourceExpr> newArgumentListsSuffix = List.nil();
							return methodWithArgs(op, newNameSuffix, newArgumentListsSuffix, SourceExprDesugarer.this);
						}
						default:
							return fallback(op);
						}
					}
				});
			}

			/**
			 * Desugar the field definition as a method that takes parameters.
			 */
			private DesugarResult<List<FunctionLiteral>> methodWithArgs(final BinaryOp call) {
				return methodWithArgs(call, Key.ANONYMOUS, List.nil(), SourceExprDesugarer.this);
			}

			/**
			 * Desugar the field definition as a method that takes parameters.
			 */
			private DesugarResult<List<FunctionLiteral>> methodWithArgs(final BinaryOp call, final Key nameSuffix, final List<SourceExpr> argumentListsSuffix, final SourceExprDesugarer ds) {
				final SourceExpr methodLeftExpr = call.getLeft();
				final SourceExpr argsExpr = call.getRight();
				return nonNull(methodLeftExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<FunctionLiteral>>>() {
					@Override
					public DesugarResult<List<FunctionLiteral>> binaryOp(BinaryOp methodDefBOp) {
						switch(methodDefBOp.getOperator()) {
						case PROJECTION: return projection(methodDefBOp);
						case JUXTAPOSITION: return juxtaposition(methodDefBOp);
						default: return fallback(methodDefBOp);
						}
					}

					/**
					 * When we find a projection in the signature that means they are specifying a "self arg" - the name
					 * of the receiver of the method call.
					 */
					private DesugarResult<List<FunctionLiteral>> projection(BinaryOp methodDefBOp) {
						final SourceExpr nameSourceExpr = methodDefBOp.getRight();
						final SourceExpr selfNameSourceExpr = methodDefBOp.getLeft();
						final Key selfName = expectIdentifier(selfNameSourceExpr);
						return apply(nameSourceExpr, selfName, ds);
					}

					/**
					 * Desugar a method signature with multiple argument lists.  The argument list we are getting
					 * should be pre-pended to the argument list we already have (if any?)
					 *
					 * The juxtaposition should split a signature like this:
					 *
					 * ex: a(b) c(d)
					 * <pre>
					 * a  (b)
					 * \  /
					 * CALL   c
					 *   \    /
					 *  JUXTAPOSITION  (d)
		 			 *              \  /
					 *              CALL
					 * </pre>
					 */
					private DesugarResult<List<FunctionLiteral>> juxtaposition(final BinaryOp methodDefBOp) {
						// for a(b)c(d)
						// argsExpr = (d)
						// methodDefBOp.right = id(c)
						// methodDefBOp.left = call(id(a),(b))
						return methodDefBOp.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<FunctionLiteral>>>() {
							@Override
							public DesugarResult<List<FunctionLiteral>> fallback(SourceExpr other) {
								BadCoreExpr problem = new BadCoreExpr(methodDefBOp.getSourceFileRanges(), "Invalid method signature");
								return ds.withValue(methods.snoc(FunctionLiteral.property(Key.ANONYMOUS, problem)));
							}

							@Override
							public DesugarResult<List<FunctionLiteral>> binaryOp(BinaryOp op) {
								switch(op.getOperator()) {
								case CALL: {
									Key key = expectIdentifier(methodDefBOp.getRight());
									Key newNameSuffix = concatNameParts(key, nameSuffix);
									List<SourceExpr> newArgumentListsSuffix = argumentListsSuffix.cons(argsExpr);
									return methodWithArgs(op, newNameSuffix, newArgumentListsSuffix, ds);
								}
								default:
									return fallback(op);
								}
							}
						});
					}


					DesugarResult<List<FunctionLiteral>> apply(SourceExpr nameExpr, Key selfName, SourceExprDesugarer ds) {
						final Key key = expectIdentifier(nameExpr);
						final DesugarResult<FunctionLiteral> methodDs = ds.method(
								methodSourceExpr,
								concatNameParts(key, nameSuffix),
								flattenArgumentLists(argumentListsSuffix.cons(argsExpr)),
								selfName,
								body);
						return methodDs.withValue(methods.cons(methodDs.getValue()));
					}
					@Override
					public DesugarResult<List<FunctionLiteral>> fallback(SourceExpr other) {
						return apply(methodLeftExpr, Key.ANONYMOUS, SourceExprDesugarer.this);
					}
				}));
			}

			/**
			 * Desugar the field as a method that doesn't take any parameters
			 */
			private DesugarResult<List<FunctionLiteral>> methodWithGuarantee(BinaryOp targetBOp) {
				final SourceExpr newLvalueExpr = targetBOp.getLeft();
				final DesugarResult<CoreExpr> newGuaranteeDs = expr(targetBOp.getRight());
				final CoreExpr combinedGuarantee = composeGuarantees(postcondition, newGuaranteeDs.getValue());
				return newGuaranteeDs.addMethod(methodSourceExpr, newLvalueExpr, body, combinedGuarantee, methods);
			}

			/**
			 * In this case the lvalue wasn't something we recognize normally so it should be an identifier, which is the name of the
			 * method.
			 */
			@Override
			public DesugarResult<List<FunctionLiteral>> fallback(SourceExpr other) {
				final Key key = expectIdentifier(signatureSourceExpr);
				final FunctionLiteral method = FunctionLiteral.property(key, body);
				return withValue(nonNull(methods.cons(method)));
			}
		});
		return nonNull(result);
	}

	/**
	 * Desugar something the MUST be an identifier.  If it's not an identifier, a placeholder
	 * is returned - an instance of BadIdentifier - that can be used as an identifier to continue
	 * with desugaring but which will be reported as an error.
	 */
	protected static Key expectIdentifier(final SourceExpr sourceExpr) {
		return sourceExpr.acceptVisitor(new BaseSourceExprVisitor<Key>() {
			@Override
			public Key key(Key key) {
				return key;
			}

			@Override
			public Key fallback(SourceExpr other) {
				return new BadIdentifier(sourceExpr);
			}
		});
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
		if(guarantee.equals(FunctionLiteral.EMPTY_POSTCONDITION)) {
			return newGuarantee;
		} else if(newGuarantee.equals(FunctionLiteral.EMPTY_POSTCONDITION)) {
			return guarantee;
		} else {
			return Call.operator(guarantee, Operator.LOGICAL_AND, newGuarantee);
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
	DesugarResult<CoreExpr> makeRow(SourceExpr sourceExpr, List<SourceExpr> headings) {
		final List<SourceExpr> values = flattenCommas(stripParens(sourceExpr));
		final DesugarResult<List<FunctionLiteral>> methodsDs = headings.zip(values).foldRight(new F2<P2<SourceExpr,SourceExpr>, DesugarResult<List<FunctionLiteral>>, DesugarResult<List<FunctionLiteral>>>() {
			@Override
			public DesugarResult<List<FunctionLiteral>> f(
					P2<SourceExpr, SourceExpr> p,
					DesugarResult<List<FunctionLiteral>> ds) {
				if(p == null || ds == null) throw new NullPointerException();
				final SourceExpr headingExpr = nonNull(p._1());
				final SourceExpr cellSourceExpr = nonNull(p._2());
				final List<FunctionLiteral> tailMethods = ds.getValue();
				final DesugarResult<CoreExpr> cellDs = ds.expr(cellSourceExpr);
				return cellDs.addMethod(null, headingExpr, cellDs.getValue(), FunctionLiteral.EMPTY_POSTCONDITION, tailMethods);
			}
		}, this.withValue(List.nil()));

		// TODO Report extra fields?
		// TODO Report missing fields?
		return methodsDs.withDesugared(sourceExpr, new ObjectLiteral(sourceExpr.getSourceFileRanges(), methodsDs.getValue()));
	}

	/**
	 * If the expression is wrapped in parentheses, remove them and return a new child expression with an offset reflecting
	 * the offset to the expression inside the parentheses.
	 */
	private SourceExpr stripParens(SourceExpr sourceExpr) {
		return nonNull(sourceExpr.acceptVisitor(new BaseSourceExprVisitor<SourceExpr>() {
			@Override
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

	protected DesugarResult<CoreExpr> functionLiteral(final SourceExpr sourceExpr,
			final SourceExpr argsSourceExpr, final SourceExpr bodySourceExpr, final CoreExpr body) {
		return nonNull(argsSourceExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
			@Override
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				return functionLiteral(sourceExpr, argsSourceExpr, Key.ANONYMOUS, body);
			}

			@Override
			public DesugarResult<CoreExpr> binaryOp(BinaryOp op) {
				if(op.getOperator() == Operator.CALL) {
					final Key selfName = expectIdentifier(op.getLeft());
					final SourceExpr args = op.getRight();
					return functionLiteral(sourceExpr, args, selfName, body);
				} else {
					return fallback(op);
				}
			}
		}));
	}

	protected DesugarResult<CoreExpr> functionLiteral(SourceExpr sourceExpr, final SourceExpr argsSourceExpr,
			final Key selfName, final CoreExpr body) {
		final DesugarResult<FunctionLiteral> methodDs = method(sourceExpr, Key.ANONYMOUS, argsSourceExpr, selfName, body);
		return methodDs.withValue((CoreExpr)new ObjectLiteral(NOT_FROM_SOURCE, methodDs.getValue()), sourceExpr);
	}

	/**
	 * Create a function literal from an args definition (still in source form) and a body (already desugared).
	 * @param args Argument source expression
	 * @param body Function body as a core expression
	 * @param sourceOffset Absolute source offset of sourceExpr
	 * @param argsSourceOffset Absolute offset in characters to the args
	 * @param bodySourceOffset Absolute source offset of the function body expression
	 * @param guaranteeSourceOffset Absolute source offset of the guarantee; use the same offset as sourceOffset if none specified
	 */
	protected DesugarResult<FunctionLiteral> method(final SourceExpr methodSourceExpr, final Key methodName, final SourceExpr args, final Key selfName, final CoreExpr body) {
		return nonNull(args.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<FunctionLiteral>>() {
			@Override
			public DesugarResult<FunctionLiteral> binaryOp(BinaryOp op) {
				switch(op.getOperator()) {
				case MEMBER_OF:
					// (x,y):Guarantee = ...
					return applyGuarantee(op);
				default:
					return fallback(op);
				}
			}

			private DesugarResult<FunctionLiteral> applyGuarantee(BinaryOp op) {
				final SourceExpr newArgs = op.getLeft();
				final SourceExpr newGuaranteeSourceExpr = op.getRight();
				final DesugarResult<CoreExpr> newGuaranteeDs = expr(newGuaranteeSourceExpr);
				CoreExpr newBody = SourceExprDesugarer.this.applyGuarantee(body, op.getOperator(), newGuaranteeDs.getValue());
				return newGuaranteeDs.method(methodSourceExpr, methodName, newArgs, selfName, newBody);
			}

			@Override
			public DesugarResult<FunctionLiteral> fallback(SourceExpr other) {
				final List<SourceExpr> exprs = flattenCommas(stripParens(args));
				return method(methodSourceExpr, methodName, single(exprs), selfName, body);
			}
		}));

	}

	/**
	 * Take a list of SourceExpr that might be a comma-separated list of arguments and make that into
	 * a list of arguments lists with the arguments split by commas.
	 */
	List<List<SourceExpr>> flattenArgumentLists(List<SourceExpr> a) {
		return a.map(x -> flattenCommas(x));
	}

	public DesugarResult<FunctionLiteral> method(SourceExpr methodSourceExpr, Operator operator, SourceExpr other, Key selfArg, CoreExpr body) {
		return method(methodSourceExpr, opMethodName(operator), other, selfArg, body);
	}

	protected DesugarResult<FunctionLiteral> method(SourceExpr methodSourceExpr, final Key methodName, List<List<SourceExpr>> argumentListsSourceExprs, Key selfName, CoreExpr body) {
		return method(methodSourceExpr, methodName, argumentListsSourceExprs, selfName, FunctionLiteral.EMPTY_PRECONDITION, body, FunctionLiteral.EMPTY_POSTCONDITION);
	}

	protected DesugarResult<FunctionLiteral> method(final SourceExpr methodSourceExpr, final Key methodName, List<List<SourceExpr>> argumentListsSourceExprs, Key selfName, CoreExpr precondition, CoreExpr currBody, CoreExpr postcondition) {
		final DesugarResult<P3<CoreExpr,CoreExpr,List<List<Key>>>> argsDs = argumentListsSourceExprs.zipIndex().foldRight(new F2<P2<List<SourceExpr>,Integer>, DesugarResult<P3<CoreExpr,CoreExpr,List<List<Key>>>>, DesugarResult<P3<CoreExpr,CoreExpr,List<List<Key>>>>>() {
			@Override
			public DesugarResult<P3<CoreExpr,CoreExpr, List<List<Key>>>> f(
					P2<List<SourceExpr>, Integer> a,
					DesugarResult<P3<CoreExpr, CoreExpr, List<List<Key>>>> argumentListsDsState) {
				if(a == null) throw new NullPointerException();
				if(argumentListsDsState == null) throw new NullPointerException();
				List<SourceExpr> argsListSourceExprs = a._1();
				final int argListNumber = a._2();
				P3<CoreExpr, CoreExpr, List<List<Key>>> s = argumentListsDsState.getValue();
				CoreExpr tempPrecondition = s._1();
				CoreExpr tempBody = s._2();
				List<List<Key>> argLists = s._3();
				DesugarResult<P3<CoreExpr,CoreExpr,List<Key>>> argListDs = argsListSourceExprs.zipIndex().foldRight(new F2<P2<SourceExpr,Integer>, DesugarResult<P3<CoreExpr,CoreExpr,List<Key>>>, DesugarResult<P3<CoreExpr,CoreExpr,List<Key>>>>() {
					@Override
					public DesugarResult<P3<CoreExpr,CoreExpr,List<Key>>> f(P2<SourceExpr,Integer> argExprWithIndex,
							DesugarResult<P3<CoreExpr,CoreExpr,List<Key>>> argListDsState) {
						if(argListDsState == null) throw new NullPointerException();
						if(argExprWithIndex == null) throw new NullPointerException();
						final int argNumber = argExprWithIndex._2();
						final SourceExpr argExpr = argExprWithIndex._1();
						final int index = argExprWithIndex._2();

						// Need another fold because there are multiple parameter lists.

						CoreExpr precondition = argListDsState.getValue()._1();
						CoreExpr body = argListDsState.getValue()._2();
						List<Key> paramList = argListDsState.getValue()._3();
						final DesugarResult<P3<CoreExpr, CoreExpr, List<Key>>> paramDs = argListDsState.methodFormalArgument(
								methodSourceExpr,
								paramList,
								argExpr,
								precondition,
								body,
								index,
								argListNumber
						);
						return paramDs;
					}
				}, argumentListsDsState.withValue(P.p(tempPrecondition, tempBody, List.nil())));

				CoreExpr newPrecondition = argListDs.getValue()._1();
				CoreExpr newBody = argListDs.getValue()._2();
				return argListDs.withValue(P.p(newPrecondition, newBody, argLists.conss(argListDs.getValue()._3())));
			}
		}, this.withValue(P.p(precondition, currBody, List.nil())));

		final List<List<Key>> processedArgs = nonNull(argsDs.getValue()._3());
		final CoreExpr newPrecondition = nonNull(argsDs.getValue()._1());
		final CoreExpr newBody = nonNull(argsDs.getValue()._2());
		List<SourceFileRange> methodSourceFileRanges = methodSourceExpr!=null ? methodSourceExpr.getSourceFileRanges() : NOT_FROM_SOURCE;
		final FunctionLiteral method = new FunctionLiteral(methodSourceFileRanges, selfName, methodName, processedArgs, newPrecondition, newBody, postcondition);
		if(methodSourceExpr != null)
			return argsDs.withDesugared(method, methodSourceExpr);
		else
			return argsDs.withValue(method);
	}

	/**
	 * Desugaring a parameter declaration in a method's formal parameter list.
	 * @param paramList List of formal parameters; this is mutated to add the parameter(s) to the end
	 * @param argExpr The source expression for the formal argument declaration
	 * @param bodyDs The desugared function body - this might be wrapped in another function while unpacking parameters
	 * @param index TODO
	 * @return The result of the desugaring includes the new source mappings and the new method body
	 */
	public DesugarResult<P3<CoreExpr, CoreExpr, List<Key>>> methodFormalArgument(final SourceExpr methodSourceExpr, final List<Key> paramList, final SourceExpr argExpr, final CoreExpr precondition, final CoreExpr body, final int index, final int index2) {
		final SourceExprDesugarer ds = SourceExprDesugarer.this;
		return nonNull(argExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P3<CoreExpr, CoreExpr, List<Key>>>>() {

			@Override
			public DesugarResult<P3<CoreExpr, CoreExpr, List<Key>>> unaryOp(UnaryOp argOp) {
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
			public DesugarResult<P3<CoreExpr, CoreExpr, List<Key>>> unpackObject(UnaryOp pattern) {
				final SourceExpr field = pattern.getOperand();
				final DesugarResult<List<P2<SourceExpr, SourceExpr>>> pairsDs = collectFields(ds, field, List.nil());
				final P2<List<SourceExpr>, List<SourceExpr>> lists = List.unzip(pairsDs.getValue());
				final List<SourceExpr> tempParams = lists._2();
				final DesugarResult<FunctionLiteral> tempMethodDs = pairsDs.method(methodSourceExpr, Key.ANONYMOUS, single(tempParams), Key.ANONYMOUS, body);
				FunctionLiteral tempMethod = tempMethodDs.getValue();
				final Key objectTmpName = tempMethod.getArgumentLists().head().head();
				final DesugarResult<List<CoreExpr>> argsDs = lists._1().foldRight(new F2<SourceExpr, DesugarResult<List<CoreExpr>>, DesugarResult<List<CoreExpr>>>() {

					@Override
					public DesugarResult<List<CoreExpr>> f(SourceExpr a, DesugarResult<List<CoreExpr>> b) {
						if(a == null) throw new NullPointerException();
						if(b == null) throw new NullPointerException();
						DesugarResult<CoreExpr> projectionDs = b.projection(a, (SourceExpr)objectTmpName, a, false, false);
						return projectionDs.withValue(b.getValue().cons(projectionDs.getValue()));
					}

				}, pairsDs.withValue(List.nil()));
				List<CoreExpr> args = argsDs.getValue();
				return wrapper(paramList, precondition, argsDs, tempMethod, objectTmpName, args);
			}

			public DesugarResult<List<P2<SourceExpr, SourceExpr>>> collectFields(final SourceExprDesugarer ds, final SourceExpr field, final List<P2<SourceExpr, SourceExpr>> currFields) {
				return nonNull(field.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<P2<SourceExpr, SourceExpr>>>>() {
					/**
					 *
					 * @param argSourceExpr For x = y, this would be the expression for x
					 * @param parameterPattern For x = y, this would be the expression for y
					 * @return
					 */
					DesugarResult<List<P2<SourceExpr, SourceExpr>>> field(SourceExpr argSourceExpr, final SourceExpr parameterPattern) {
						return ds.withValue(nonNull(currFields.cons(P.p(argSourceExpr, parameterPattern))));
					}

					@Override
					public DesugarResult<List<P2<SourceExpr, SourceExpr>>> key(Key key) {
						return field(key, key);
					}

					@Override
					public DesugarResult<List<P2<SourceExpr, SourceExpr>>> binaryOp(BinaryOp op) {
						switch(op.getOperator()) {
						case ASSIGNMENT: {
							// Field name = variable name
							return field(op.getLeft(), op.getRight());
						}
						case COMMA:
						case JUXTAPOSITION:
						case NEWLINE: {
							final DesugarResult<List<P2<SourceExpr, SourceExpr>>> rightDs = collectFields(ds, op.getRight(), currFields);
							final DesugarResult<List<P2<SourceExpr, SourceExpr>>> leftDs = collectFields(rightDs, op.getLeft(), rightDs.getValue());
							return leftDs;
						}

						default:
							return fallback(op);
						}
					}

					@Override
					public DesugarResult<List<P2<SourceExpr, SourceExpr>>> fallback(SourceExpr op) {
						// Unexpected whatever-it-is
						return field(op, op);
					}
				}));
			}

			/**
			 * If we find square brackets we are pulling elements from a sequence.
			 *
			 * { method([a,b,c,d]) = body }
			 * becomes
			 * { method(_abcd) = ((a,b,c,d) -> body)(_abcd[0], _abcd[1], _abcd[2], _abcd[3]) }
			 */
			public final DesugarResult<P3<CoreExpr, CoreExpr, List<Key>>> unpackList(UnaryOp pattern) {
				final List<SourceExpr> tempParams = flattenCommas(pattern.getOperand());
				if(tempParams.isEmpty()) {
					// Actually more of a precondition
					throw new UnsupportedOperationException("Not implemented");
				} else {
					final DesugarResult<FunctionLiteral> tempMethodDs = ds.method(methodSourceExpr, Key.ANONYMOUS, single(tempParams), Key.ANONYMOUS, body);
					FunctionLiteral tempMethod = tempMethodDs.getValue();
					final Key listTmpName = nonNull(tempMethod.getArgumentLists().head().head());
					List<CoreExpr> args = List.nil();
					for(int n=tempParams.length()-1; n >= 0; n--) {
						args = args.cons(new Call(NOT_FROM_SOURCE, listTmpName, new Identifier("get"), new NumberLiteral(n)));
					}
					return wrapper(paramList, precondition, tempMethodDs, tempMethod, listTmpName, args);
				}
			}

			private DesugarResult<P3<CoreExpr, CoreExpr, List<Key>>> wrapper(
					final List<Key> paramList, final CoreExpr precondition,
					final SourceExprDesugarer ds, FunctionLiteral tempMethod,
					final Key paramName, List<CoreExpr> args) {
				final ObjectLiteral func = new ObjectLiteral(tempMethod);
				final Call newBody = Call.callFunction(func, args);
				CoreExpr newPrecondition = composePreconditions(precondition, tempMethod.getPrecondition());
				return ds.withValue(P.p(newPrecondition, (CoreExpr)newBody, cons(paramName, paramList)));
			}

			public final DesugarResult<P3<CoreExpr, CoreExpr, List<Key>>> unpackLazy(UnaryOp pattern) {
				final List<SourceExpr> tempParams = List.single(pattern.getOperand());
				final DesugarResult<FunctionLiteral> tempMethodDs = ds.method(methodSourceExpr, Key.ANONYMOUS, single(tempParams), Key.ANONYMOUS, body);
				FunctionLiteral tempMethod = tempMethodDs.getValue();
				final Key lazyTmpName = nonNull(tempMethod.getArgumentLists().head().head()); // We know the temp method should accept at least one parameter, I think
				List<CoreExpr> args = List.single(Call.callFunction(lazyTmpName, List.nil()));
				return wrapper(paramList, precondition, tempMethodDs, tempMethod, lazyTmpName, args);
			}

			@Override
			public DesugarResult<P3<CoreExpr, CoreExpr, List<Key>>> binaryOp(final BinaryOp argOp) {
				switch(argOp.getOperator()) {
				case MEMBER_OF:
				case EQ:
				case GE:
				case GT:
				case LE:
				case LT:
				case NEQ:
					final Key paramDecl = expectIdentifier(argOp.getLeft());
					final DesugarResult<CoreExpr> assertionDs = expr(argOp.getRight());
					// Check parameter precondition
					CoreExpr newPrecondition = insertContract(precondition, paramDecl, argOp.getOperator(), assertionDs.getValue());
					return assertionDs.withValue(P.p(newPrecondition, body, cons(paramDecl, paramList)));

				default:
					return fallback(argOp);
				}
			}

			@Override
			public DesugarResult<P3<CoreExpr, CoreExpr, List<Key>>> key(Key key) {
				return ds.withValue(P.p(precondition, body, cons(key, paramList)));
			}
			@Override
			public DesugarResult<P3<CoreExpr, CoreExpr, List<Key>>> fallback(SourceExpr other) {
				return key(new BadIdentifier(other.getSourceFileRanges(), "Invalid argument specification", other.toSource()));
			}
		}));
	}

	/**
	 *
	 * (x ∈ y) -> x
	 *
	 * to
	 *
	 * (x) -> assert(x ∈ y) in (x)
	 *
	 * @param currentPrecondition
	 * @param parameterName
	 * @param operator
	 * @param constraint
	 * @return
	 */
	protected CoreExpr insertContract(CoreExpr currentPrecondition, Key parameterName,
			Operator operator, CoreExpr constraint) {
		Call check = new Call(constraint.getSourceFileRanges(), parameterName, opMethodName(operator), constraint);
		return composePreconditions(currentPrecondition, check);
	}

	private CoreExpr composePreconditions(CoreExpr a, CoreExpr b) {
		if(a.equals(FunctionLiteral.EMPTY_PRECONDITION)) {
			return b;
		} else if(b.equals(FunctionLiteral.EMPTY_PRECONDITION)){
			return a;
		} else {
			return Call.operator(a, Operator.LOGICAL_AND, b);
		}
	}

	protected CoreExpr applyGuarantee(CoreExpr currentPostcondition, Operator operator, CoreExpr constraint) {
		throw new Error("Not implemented ...");
	}

	private List<SourceExpr> flattenList(final SourceExpr arg, final Operator sep) {

		final List<SourceExpr> result = nonNull(arg.acceptVisitor(new BaseSourceExprVisitor<List<SourceExpr>>() {
			@Override
			public List<SourceExpr> emptyExpr(EmptyExpr emptyExpr) {
				// Don't add anything for an empty expression
				return List.nil();
			}

			@Override
			public List<SourceExpr> binaryOp(BinaryOp op) {
				if(op.getOperator() == sep || op.getOperator() == Operator.NEWLINE || op.getOperator() == Operator.JUXTAPOSITION) {

					final List<SourceExpr> left = flattenList(op.getLeft(), sep);

					final List<SourceExpr> right = flattenList(op.getRight(), sep);
					return left.isEmpty() ? right :
						right.isEmpty() ? left :
							left.tail().isEmpty() ? cons(left.head(), right) :
								left.append(right);
				} else {
					return fallback(op);
				}
			}

			@Override
			public List<SourceExpr> fallback(SourceExpr other) {
				return single(arg);
			}
		}));
		return result;
	}

	private List<SourceExpr> flattenCommas(final SourceExpr arg) {
		return flattenList(arg, Operator.COMMA);
	}
	protected DesugarResult<CoreExpr> binaryOpToMethodCall(final BinaryOp op, boolean optional) {
		final SourceExpr leftSourceExpr = op.getLeft();
		final Operator operator = op.getOperator();
		List<SourceFileRange> operatorRanges = op.getOperatorRanges();
		final SourceExpr rightSourceExpr = op.getRight();
		return binaryOpToMethodCall(op, leftSourceExpr, operator, operatorRanges, rightSourceExpr, optional);
	}

	protected DesugarResult<CoreExpr> binaryOpToMethodCall(SourceExpr op, final SourceExpr leftSourceExpr, final Operator operator, List<SourceFileRange> operatorRanges, final SourceExpr rightSourceExpr, boolean optional) {
		final DesugarResult<CoreExpr> leftDs = expr(leftSourceExpr);
		final DesugarResult<CoreExpr> rightDs = leftDs.expr(rightSourceExpr);
		return rightDs.withDesugared(op, binaryOpToMethodCall(op.getSourceFileRanges(), leftDs.getValue(), operator, operatorRanges, rightDs.getValue(), optional));
	}

	protected Call binaryOpToMethodCall(List<SourceFileRange> ranges, final CoreExpr leftCoreExpr, final Operator operator,	List<SourceFileRange> operatorRanges, final CoreExpr rightCoreExpr, boolean optional) {
		final boolean rightAssoc = operator.isRightAssociative();
		final CoreExpr target = rightAssoc?rightCoreExpr:leftCoreExpr;
		final CoreExpr parameter = rightAssoc?leftCoreExpr:rightCoreExpr;
		return new Call(ranges, target, opMethodName(operatorRanges, operator), optional, parameter);
	}


	protected DesugarResult<CoreExpr> lazyValue(SourceExpr sourceExpr, SourceExpr body) {
		final DesugarResult<CoreExpr> bodyDs = expr(body);
		return bodyDs.functionLiteral(sourceExpr, EmptyExpr.SYNTHETIC_INSTANCE, body, bodyDs.getValue());
	}

	protected DesugarResult<CoreExpr> singletonListLiteral(UnaryOp op) {
		final SourceExpr operandSourceExpr = op.getOperand();
		final DesugarResult<CoreExpr> operandDs = expr(operandSourceExpr);
		return operandDs.withDesugared(op, new ListLiteral(op.getSourceFileRanges(), single(operandDs.getValue())));
	}

//	private DesugarResult<CoreExpr> exprPair(final BinaryOp pairOp) {
//		final DesugarResult<CoreExpr> bodyDs = expr(pairOp.getRight());
//		return bodyDs.exprPair(pairOp, pairOp.getLeft(), pairOp.getRight(), bodyDs.getValue());
//	}
//
//	public DesugarResult<CoreExpr> exprPair(final SourceExpr pairOp, final SourceExpr curr, final SourceExpr bodySourceExpr, final CoreExpr body) {
//		return nonNull(curr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
//			@Override
//			public DesugarResult<CoreExpr> binaryOp(BinaryOp leftOp) {
//				switch(leftOp.getOperator()) {
//				case MONAD_EXTRACT: return monadExtract(pairOp, bodySourceExpr, body, leftOp);
//				case ASSIGNMENT: return let(pairOp, bodySourceExpr, body, leftOp);
//
//				default:
//					return fallback(leftOp);
//				}
//			}
//
//			@Override
//			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
//				return monadAndThen(pairOp, bodySourceExpr, body, other);
//			}
//
//		}));
//	}
//
//	/**
//	 * In this case the left-hand expression isn't a kind of let so we don't have to introduce a new variable.  This
//	 * is the "and then" monad operation, denoted ">>" in Haskell but we'll use ";" since ">>" is so bit-shifty.
//	 * We have to make the next step lazy, too, to allow the monad to do control flow.
//	 *
//	 * x ; y == (x).";"(-> y)
//	 */
//	public DesugarResult<CoreExpr> monadAndThen(
//			final SourceExpr pairOp, final SourceExpr bodySourceExpr,
//			final CoreExpr body, SourceExpr other) {
//		final DesugarResult<CoreExpr> rhsDs = expr(other);
//		final DesugarResult<CoreExpr> contDs = rhsDs.functionLiteral(null, EmptyExpr.SYNTHETIC_INSTANCE, bodySourceExpr, body);
//		return contDs.withDesugared(pairOp, new Call(pairOp.getSourceFileRanges(), rhsDs.getValue(), opMethodName(Operator.SEMICOLON), contDs.getValue()));
//	}

	protected DesugarResult<CoreExpr> listLiteral(SourceExpr sourceExpr, final SourceExpr elementsExpr) {
		final List<SourceExpr> exprs = flattenCommas(elementsExpr);
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
//			case SEMICOLON:
//			case NEWLINE: {
//				return exprPair(op);
//			}
			case CALL: return call(op);
			case PIPE_TO: return pipeTo(op);
			case PIPE_FROM: return pipeFrom(op);

			// '.' and variants with NO parameters.  When there's a call, these are
			// checked for specially inside of call().
			case CALL_NEXT_METHOD: return projection(op, true, false);
			case OPT_CALL_NEXT_METHOD: return projection(op, true, true);
			case FUNCTION: return functionLiteral(op);
			case PROJECTION: return projection(op, false, false);
			case OPT_PROJECTION: return projection(op, false, true);
			case MAP_PROJECTION: return mapProjection(op, false, false);
			case MAP_OPT_PROJECTION: return mapProjection(op, false, true);


			// Normal operators are translated into a method call
			case GT:
			case LT:
			case GE:
			case LE:
			case NEQ:
			case EQ:
			case CMP:
			case POW:
			case MUL:
			case DIV:
			case ADD:
			case SUB:
			case INTERSECT:
			case XOR:
			case UNION:
			case MEMBER_OF:
			case LOGICAL_AND:
			case LOGICAL_OR:
			case FALLBACK:
				return binaryOpToMethodCall(op, false);

			case EXTEND:
				return extend(op);

			case JUXTAPOSITION:
				return juxtaposition(op);

			case LET:
				return let(op);
			default:
				return withDesugared(op, new BadCoreExpr(op.getSourceFileRanges(), "Operator not supported here: '"+op.getOperator()+"'"));
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
				return parens(op, operandSourceExpr);

			case NOT:
			case COMPLEMENT:
			case PLUS:
			case NEGATE:
			case ABSVALUE:
				return unaryOpToMethodCall(op);

			case INSPECT:
				return inspect(op);

			case SELECTOR:
				return selector(op);

			case INVALID:
				return withDesugared(op, new BadCoreExpr(op.getSourceFileRanges(), "Invalid unary operator"));
			default:
				return withDesugared(op, new BadCoreExpr(op.getSourceFileRanges(), "Operator not supported here: '"+op.getOperator()+"'"));

			}
		}

		@Override
		public DesugarResult<CoreExpr> stringLiteral(StringLiteral stringLiteral) {
			return withDesugared(stringLiteral, (CoreExpr)stringLiteral);
		}

		@Override
		public DesugarResult<CoreExpr> numberLiteral(NumberLiteral numberLiteral) {
			return withDesugared(numberLiteral, (CoreExpr)numberLiteral);
		}

		@Override
		public DesugarResult<CoreExpr> identifier(Identifier simpleName) {
			return withDesugared(simpleName, (CoreExpr)simpleName);
		}

		@Override
		public DesugarResult<CoreExpr> operator(OperatorRef operatorRef) {
			return withDesugared(operatorRef, operatorRef);
		}

		//	@Override
		//	public CoreExpr visitWhitespace(Whitespace ws) {
		//		return null;
		//	}
		//
		//	@Override
		//	public CoreExpr visitComment(Comment c) {
		//		return null;
		//	}
		//
		//	@Override
		//	public CoreExpr visitEof() {
		//		return null;
		//	}

		@Override
		public DesugarResult<CoreExpr> badSourceExpr(BadSourceExpr badSourceExpr) {
			return withDesugared(badSourceExpr, new BadCoreExpr(badSourceExpr.getSourceFileRanges(), badSourceExpr.getMessage()));
		}
		@Override
		public DesugarResult<CoreExpr> emptyExpr(EmptyExpr emptyExpr) {
			return withDesugared(emptyExpr, new BadCoreExpr(emptyExpr.getSourceFileRanges(), "Expected expression"));
		}

		@Override
		public DesugarResult<CoreExpr> badIdentifier(BadIdentifier badIdentifier) {
			return withDesugared(badIdentifier, new BadCoreExpr(badIdentifier.getSourceFileRanges(), badIdentifier.getMessage()));
		}

		@Override
        public DesugarResult<CoreExpr> mixfixFunctionIdentifier(
                MixfixFunctionIdentifier id) {
			return withDesugared(id, (CoreExpr)id);
        }
	}

	public DesugarResult<CoreExpr> inspect(UnaryOp op) {
		final DesugarResult<CoreExpr> exprDs = expr(op.getOperand());
		return exprDs.withDesugared(op, new Inspect(op.getSourceFileRanges(), exprDs.getValue()));
	}

	/**
	 * A variant is a special kind of callback.  Using the special variant syntax will result
	 * in an object that tries to call back, but treats it as a contract failure if the provided
	 * object does not implement the specified method or has a contract failure for calling that
	 * method:
	 *
	 * #<something> = { (x # y) = y.?<something> &&& y.<something> }
	 */
	public DesugarResult<CoreExpr> selector(UnaryOp op) {
		final Identifier y = new Identifier("//compiler/selector/x");
		P3<CoreExpr,CoreExpr,SourceExprDesugarer> ps = op.getOperand().acceptVisitor(new BaseSourceExprVisitor<P3<CoreExpr,CoreExpr,SourceExprDesugarer>>() {
			@Override
			public P3<CoreExpr,CoreExpr,SourceExprDesugarer> key(Key field) {
				DesugarResult<CoreExpr> preDs = projection(op, (CoreExpr)y, field, false, true);
				CoreExpr precondition = preDs.getValue();
				DesugarResult<CoreExpr> bodyDs = preDs.projection(op, (CoreExpr)y, field, false, false);
				CoreExpr body = bodyDs.getValue();
				return P.p(precondition, body, bodyDs);
			}

			@Override
			public P3<CoreExpr,CoreExpr,SourceExprDesugarer> binaryOp(BinaryOp op) {
				// Variant has arguments
				DesugarResult<CoreExpr> preDs = desugar(new BinaryOp(op.getOperator(), op.getOperatorRanges(), new BinaryOp(Operator.OPT_PROJECTION, op.getOperatorRanges(), y, op.getLeft()), op.getRight()));
				CoreExpr precondition = preDs.getValue();
				DesugarResult<CoreExpr> bodyDs = preDs.desugar(new BinaryOp(op.getOperator(), op.getOperatorRanges(), new BinaryOp(Operator.PROJECTION, op.getOperatorRanges(), y, op.getLeft()), op.getRight()));
				CoreExpr body = bodyDs.getValue();
				return P.p(precondition, body, bodyDs);
			}

			@Override
			public P3<CoreExpr, CoreExpr, SourceExprDesugarer> unaryOp(UnaryOp op) {
				// Callback shorthand - .(x) == (f) -> f(x)
				if(op.getOperator() == Operator.PARENS) {
					DesugarResult<List<List<CoreExpr>>> argListsDs = desugarArgLists(single(flattenCommas(op.getOperand())));
					List<List<CoreExpr>> argLists = argListsDs.getValue();

					CoreExpr precondition = new Call(op.getSourceFileRanges(), y, Key.ANONYMOUS, argLists, false, true);
					CoreExpr body = new Call(op.getSourceFileRanges(), y, Key.ANONYMOUS, argLists, false, false);
					return P.p(precondition, body, argListsDs);
				}
			    return super.unaryOp(op);
			}
			@Override
			public P3<CoreExpr, CoreExpr, SourceExprDesugarer> fallback(SourceExpr other) {
				final BadCoreExpr expr = new BadCoreExpr(other.getSourceFileRanges(), "Expected identifier or method invokation: "+other);
				return P.p(expr, expr, withDesugared(other, expr));
			}
		});

		SourceExprDesugarer ds = ps._3();
		CoreExpr precondition = ps._1();
		CoreExpr body = ps._2();
		FunctionLiteral method = new FunctionLiteral(NOT_FROM_SOURCE, Key.ANONYMOUS, opMethodName(Operator.CALL), single(single((Key)y)), precondition , body, FunctionLiteral.EMPTY_POSTCONDITION);
		return ds.withDesugared(op, new ObjectLiteral(method));
	}

	public DesugarResult<CoreExpr> extend(BinaryOp op) {
		final DesugarResult<CoreExpr> leftDs = expr(op.getLeft());
		final DesugarResult<CoreExpr> rightDs = leftDs.expr(op.getRight());
		return rightDs.withDesugared(op, new Extend(op.getSourceFileRanges(), leftDs.getValue(), rightDs.getValue()));
	}

	private DesugarResult<CoreExpr> let(final BinaryOp op) {
		List<SourceExpr> namedValues = nonNull(op.getLeft().acceptVisitor(new BaseSourceExprVisitor<List<SourceExpr>>() {
			@Override
			public List<SourceExpr> unaryOp(UnaryOp op) {
				if(op.getOperator() == Operator.PARENS) {
					return flattenCommas(op.getOperand());
				}
				return super.unaryOp(op);
			}

			@Override
			public List<SourceExpr> fallback(SourceExpr other) {
				return flattenCommas(other);
			}
		}));

		return desugar(op.getRight()).mapValue((ds, body) ->
			ds.let(op, namedValues, body)
		);
	}

	private CoreExpr let(final BinaryOp op, List<SourceExpr> namedValues, CoreExpr body) {
	    return (CoreExpr)namedValues.foldRight((a, ds) ->
	    	ds.mapValue((ds2,bindings) -> a.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<Key,CoreExpr>>>() {
	    			@Override
	    			public DesugarResult<P2<Key,CoreExpr>> binaryOp(BinaryOp letOp) {
	    				if(letOp.getOperator() == Operator.ASSIGNMENT) {
	    					return ds2.let(letOp);
	    				}
	    				return fallback(letOp);
	    			}

	    			@Override
	    			public DesugarResult<P2<Key,CoreExpr>> fallback(SourceExpr other) {
	    				BinaryOp letOp = new BinaryOp(other.getSourceFileRanges(), Operator.ASSIGNMENT, SourceFileRange.EMPTY_LIST, Key.ANONYMOUS, other);
	    				return ds2.localVariableDef(op, letOp, new Identifier("//compiler/let/unnamed"));
	    			}

	    }).mapValue((ds3, binding) -> ds3.withValue(bindings.cons(binding)))), withValue(List.<P2<Key,CoreExpr>>nil()))
	    .mapValue((ds, bindings) -> ds.withDesugared(op, new Let(op.getSourceFileRanges(), bindings, body)));
    }

	public DesugarResult<P2<Key, CoreExpr>> let(SourceExpr left, SourceExpr right) {
		return left.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<Key, CoreExpr>>>() {
			@Override
			public DesugarResult<P2<Key, CoreExpr>> binaryOp(BinaryOp op) {
				switch(op.getOperator()) {
				case JUXTAPOSITION:
				case CALL: {
					return localFunctionDef(op.getLeft(), op.getRight(), right);
				}
				default:
					return fallback(op);
				}
			}

			@Override
			public DesugarResult<P2<Key, CoreExpr>> fallback(SourceExpr argPattern) {
				return localVariableDef(left, right);
			}

		});
	}

	public DesugarResult<P2<Key, CoreExpr>> localVariableDef(
			final SourceExpr left,
			final SourceExpr right) {
		// x = y, z == (x -> z)(y) == {(x) = z}(y)
		Key name = expectIdentifier(left); // TODO Reimplement object unpacking :-(
		return expr(right).mapValue((rhsDs, value) -> P.p(name, value));
	}

	public DesugarResult<P2<Key, CoreExpr>> localFunctionDef(SourceExpr name, SourceExpr args, SourceExpr body) {
		return localFunctionDef(pairOp, bodySourceExpr, letOp, callOp, Key.ANONYMOUS, List.nil());
	}

	private DesugarResult<P2<Key, CoreExpr>> localFunctionDef(final SourceExpr name,
            final SourceExpr body,
            final Key name, final List<SourceExpr> argLists) {
	    // f(x) = y -->  f = {f(x) = y}
		return callOp.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<Key,CoreExpr>>>() {
			@Override
			public DesugarResult<P2<Key,CoreExpr>> fallback(SourceExpr other) {
				return makeFunction();
			}

			private DesugarResult<P2<Key,CoreExpr>> makeFunction() {
	            Key namePart = expectIdentifier(callOp.getLeft());
        		final Key selfName = concatNameParts(namePart, name);
        		List<List<SourceExpr>> argListLists = List.cons(callOp.getRight(), argLists).map(args -> flattenCommas(stripParens(args)));
        		final DesugarResult<CoreExpr> rhsDs = expr(letOp.getRight()); // y
        		final DesugarResult<FunctionLiteral> funcDs = rhsDs.method(pairOp, Key.ANONYMOUS, argListLists, selfName, FunctionLiteral.EMPTY_PRECONDITION, rhsDs.getValue(), FunctionLiteral.EMPTY_POSTCONDITION); // (x -> y)
        		return funcDs.withValue(P.p(selfName, new ObjectLiteral(funcDs.getValue())));
            }

			@Override
			public DesugarResult<P2<Key,CoreExpr>> binaryOp(BinaryOp op) {
				if(op.getOperator() == Operator.JUXTAPOSITION) {
					return juxtaposition(op);
				}
			    return super.binaryOp(op);
			}

			private DesugarResult<P2<Key,CoreExpr>> juxtaposition(BinaryOp juxtapositionOp) {
				Key namePart = expectIdentifier(juxtapositionOp.getRight());
	            return juxtapositionOp.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<Key,CoreExpr>>>() {
	            	@Override
	            	public DesugarResult<P2<Key,CoreExpr>> fallback(SourceExpr other) {
	            		return makeFunction();
	            	}

	            	@Override
	            	public DesugarResult<P2<Key,CoreExpr>> binaryOp(BinaryOp op2) {
	            		if(op2.getOperator() == Operator.CALL) {
	            			return localFunctionDef(pairOp, bodySourceExpr, letOp, op2, concatNameParts(namePart, name), List.cons(callOp.getRight(), argLists));
	            		}
	            	    return super.binaryOp(op2);
	            	}
	            });
            }
		});
    }

	private DesugarResult<CoreExpr> juxtaposition(final BinaryOp op) {
		return nonNull(op.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
			private DesugarResult<CoreExpr> nonCallJuxtaposition() {
				return SourceExprDesugarer.this.call(op);
			}

			@Override
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				return nonCallJuxtaposition();
			}
			@Override
			public DesugarResult<CoreExpr> binaryOp(final BinaryOp opLeft) {
				if(opLeft.getOperator() == Operator.CALL || opLeft.getOperator() == Operator.CALL_NEXT_METHOD) {
					return op.getRight().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
						@Override
						public DesugarResult<CoreExpr> fallback(SourceExpr other) {
							return nonCallJuxtaposition();
						}

						public SourceExprDesugarer.DesugarResult<CoreExpr> key(Key keyOnRight) {
							Operator leftOp = opLeft.getOperator();
							boolean callNext = leftOp == Operator.CALL_NEXT_METHOD || leftOp == Operator.OPT_CALL_NEXT_METHOD;
							boolean optional = leftOp == Operator.OPT_PROJECTION || leftOp == Operator.OPT_CALL_NEXT_METHOD;
							return call(opLeft, keyOnRight, single(List.nil()), callNext, optional);
						}
					});
				} else {
					return super.binaryOp(op);
				}
			}
		}));
	}

	public DesugarResult<CoreExpr> callMethod(final BinaryOp op,
			final Key moreNames,
			final List<List<SourceExpr>> moreArgumentLists,
			final boolean optional, final List<SourceExpr> argSourceExprs,
			final BinaryOp calleeOp, Key key) {
		boolean actualOptional = optional || calleeOp.getOperator() == Operator.MAP_OPT_PROJECTION;
		final Key argName = new Identifier("//compiler/map projection/."+key.toSource());
		final DesugarResult<CoreExpr> projectionDs = call(op, (CoreExpr)argName, concatNameParts(key, moreNames), cons(argSourceExprs, moreArgumentLists), false, actualOptional);
		final DesugarResult<CoreExpr> projectFuncDs = projectionDs.function(op, argName, projectionDs.getValue());
		final DesugarResult<CoreExpr> targetDs = projectFuncDs.expr(calleeOp.getLeft());
		CoreExpr target = targetDs.getValue();
		CoreExpr projectionFunc = projectFuncDs.getValue();
		final DesugarResult<CoreExpr> mapCallDs = targetDs.withDesugared(op, new Call(NOT_FROM_SOURCE, target, new Identifier("map"), projectionFunc));
		return mapCallDs;
	}

	public DesugarResult<List<List<CoreExpr>>> desugarArgList(
            List<SourceExpr> argSourceExprs, List<List<CoreExpr>> list) {
	    if(argSourceExprs == null) throw new NullPointerException();
	    final DesugarResult<List<CoreExpr>> dsArgs = argSourceExprs.foldRight(new F2<SourceExpr, DesugarResult<List<CoreExpr>>, DesugarResult<List<CoreExpr>>>() {
	    	@Override
	    	public DesugarResult<List<CoreExpr>> f(SourceExpr argSourceExpr, DesugarResult<List<CoreExpr>> a) {
	    		final DesugarResult<CoreExpr> argDs = nonNull(a).expr(nonNull(argSourceExpr));
	    		return argDs.withValue(cons(argDs.getValue(), nonNull(a).getValue()));
	    	}
	    }, withValue(List.nil()));

	    return dsArgs.withValue(cons(dsArgs.getValue(), list));
    }

	private DesugarResult<CoreExpr> unaryOpToMethodCall(final UnaryOp op) {
	    final DesugarResult<CoreExpr> operandCoreExpr = expr(op.getOperand());
	    return operandCoreExpr.withDesugared(op, new Call(op.getSourceFileRanges(), operandCoreExpr.getValue(), opMethodName(op.getOperator())));
    }


}
