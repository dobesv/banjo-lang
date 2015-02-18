package banjo.desugar;

import static banjo.parser.util.Check.nonNull;
import static fj.data.List.cons;
import static fj.data.List.single;
import banjo.dom.BadExpr;
import banjo.dom.core.BadCoreExpr;
import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.Call;
import banjo.dom.core.CoreErrorGatherer;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.Extend;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.Inspect;
import banjo.dom.core.Let;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.core.SlotReference;
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
import fj.data.Option;
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


		<TT> DesugarResult<TT> mapValue(F<T,TT> f) {
			return withValue(f.f(getValue()));
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

	protected DesugarResult<Identifier> withDesugared(SourceExpr sourceExpr, Identifier expr) {
		return new DesugarResult<Identifier>(expr, sourceExpr);
	}

	protected DesugarResult<Identifier> withIdentifier(Identifier id) {
		return new DesugarResult<Identifier>(id, null);
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
	 * @param objectExpr Left-hand side of the projection; the "base" object we're projecting from
	 * @param projectionExpr Right-hand side of the projection
	 */
	protected DesugarResult<CoreExpr> projection(final SourceExpr sourceExpr, final SourceExpr objectExpr, final SourceExpr projectionExpr) {
		final DesugarResult<CoreExpr> objectDs = expr(objectExpr);
		return objectDs.projection(sourceExpr, objectDs.getValue(), projectionExpr);
	}

	protected DesugarResult<CoreExpr> projection(final SourceExpr sourceExpr, final CoreExpr objectCoreExpr, final SourceExpr projectionExpr) {
		return nonNull(projectionExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
			@Override
			public DesugarResult<CoreExpr> identifier(Identifier id) {
				return withDesugared(sourceExpr, new SlotReference(sourceExpr.getSourceFileRanges(), objectCoreExpr, id));
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
		final Identifier argName = new Identifier("//compiler/map projection/x");
		final DesugarResult<CoreExpr> projectionDs = projection(op, (CoreExpr)argName, projection);
		final DesugarResult<CoreExpr> projectFuncDs = projectionDs.function(op, argName, projectionDs.getValue());
		final DesugarResult<CoreExpr> mapCallDs = projectFuncDs.withDesugared(op, Call.slot(target, "map", projectFuncDs.getValue()));
		return mapCallDs;
	}

	/**
	 * Create a function object - an object with a single method with a special name and the given body.
	 */
	protected DesugarResult<CoreExpr> function(SourceExpr op, Identifier funArg, CoreExpr body) {
		final FunctionLiteral applyMethod = FunctionLiteral.function(funArg, body);
		return this.<CoreExpr>withValue(applyMethod);
	}

	protected DesugarResult<CoreExpr> pipeTo(final BinaryOp op) {
		DesugarResult<CoreExpr> leftDs = desugar(op.getLeft());
		DesugarResult<CoreExpr> rightDs = leftDs.desugar(op.getRight());
		return withDesugared(op, new Call(op.getSourceFileRanges(), rightDs.getValue(), List.single(leftDs.getValue())));
	}

	protected DesugarResult<CoreExpr> pipeFrom(final BinaryOp op) {
		DesugarResult<CoreExpr> leftDs = desugar(op.getLeft());
		DesugarResult<CoreExpr> rightDs = leftDs.desugar(op.getRight());
		return withDesugared(op, new Call(op.getSourceFileRanges(), leftDs.getValue(), List.single(rightDs.getValue())));
	}

	protected static Identifier concatNameParts(Identifier prefix, Identifier suffix) {
		return new Identifier(prefix.getSourceFileRanges().append(suffix.getSourceFileRanges()), prefix.id + " _ " + suffix.id);
	}
	protected static Identifier concatNameParts(Identifier prefix, Option<Identifier> nameSuffix) {
        return nameSuffix.map((suf) -> concatNameParts(prefix, suf)).orSome(prefix);
    }

	protected DesugarResult<CoreExpr> call(final BinaryOp op) {
		DesugarResult<CoreExpr> targetDs = expr(op.getLeft());
		final List<SourceExpr> argSourceExprs = op.getOperator() == Operator.CALL ? flattenCommas(op.getRight()) : List.single(op.getRight());
		return targetDs.call(op, targetDs.getValue(), argSourceExprs);
	}

	protected DesugarResult<CoreExpr> call(final BinaryOp op, CoreExpr target, final List<SourceExpr> argSourceExprs) {
	    final SourceExpr callSourceExpr = op;
	    final SourceExpr argsSourceExpr = op.getRight();
		return call(callSourceExpr, target, argsSourceExpr, argSourceExprs);
    }

	private DesugarResult<CoreExpr> call(final SourceExpr callSourceExpr,
            CoreExpr target, final SourceExpr argsSourceExpr,
            final List<SourceExpr> argSourceExprs) {
	    return elements(
				argsSourceExpr,
				argSourceExprs,
				null,
				argsDs -> argsDs.withDesugared(callSourceExpr, new Call(callSourceExpr.getSourceFileRanges(), target, argsDs.getValue()))
		);
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
	protected DesugarResult<CoreExpr> elements(SourceExpr sourceExpr, final List<SourceExpr> list, final Operator requireBullet, F<DesugarResult<List<CoreExpr>>, DesugarResult<CoreExpr>> cb) {

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
		final DesugarResult<List<P2<Identifier,CoreExpr>>> methodsDs = methodSourceExprs.foldRight(new F2<SourceExpr,DesugarResult<List<P2<Identifier,CoreExpr>>>,DesugarResult<List<P2<Identifier,CoreExpr>>>>() {
			@Override
			public DesugarResult<List<P2<Identifier,CoreExpr>>> f(SourceExpr methodSourceExpr, DesugarResult<List<P2<Identifier,CoreExpr>>> ds) {
				return ds.addMethod(methodSourceExpr, List.nil(), ds.getValue());
			}
		}, this.withValue(List.nil()));
		return methodsDs.withDesugared(sourceExpr, new ObjectLiteral(sourceExpr.getSourceFileRanges(), methodsDs.getValue()));
	}

	protected DesugarResult<List<P2<Identifier,CoreExpr>>> addMethod(final SourceExpr fieldSourceExpr, final List<SourceExpr> headings, final List<P2<Identifier,CoreExpr>> slots) {
		return nonNull(fieldSourceExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<P2<Identifier,CoreExpr>>>>() {
			@Override
			public DesugarResult<List<P2<Identifier,CoreExpr>>> binaryOp(BinaryOp op) {
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

			private DesugarResult<List<P2<Identifier,CoreExpr>>> visitPair(BinaryOp fieldOp, Operator combiningOp) {
				final SourceExpr left = fieldOp.getLeft();
				final SourceExpr right = fieldOp.getRight();
				return pair(left, right, combiningOp);
			}

			@Override
			public DesugarResult<List<P2<Identifier,CoreExpr>>> unaryOp(UnaryOp op) {
				switch(op.getOperator()) {
				case TABLE_HEADER: return visitTableHeader(op);
				default: return fallback(op);
				}
			}

			private DesugarResult<List<P2<Identifier,CoreExpr>>> visitTableHeader(UnaryOp headerOp) {
				throw new Error("Headings not supported ...");
			}

			@Override
			public DesugarResult<List<P2<Identifier,CoreExpr>>> identifier(Identifier id) {
				return pair(id, id, Operator.ASSIGNMENT);
			}

			private DesugarResult<List<P2<Identifier,CoreExpr>>> pair(SourceExpr lvalueExpr, SourceExpr valueSourceExpr, Operator combiningOp) {
				final DesugarResult<CoreExpr> eltDs = element(valueSourceExpr, headings);
				// TODO Apply combiningOp operator ...
				return eltDs.addMethod(fieldSourceExpr, lvalueExpr, eltDs.getValue(), Identifier.TRUE, slots);
			}


			@Override
			public DesugarResult<List<P2<Identifier,CoreExpr>>> fallback(SourceExpr other) {
				return addMethod(fieldSourceExpr, new Identifier(other.getSourceFileRanges(), other.toSource()), new BadCoreExpr(other.getSourceFileRanges(), "Expected method definition"), Identifier.TRUE, slots);
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

	Identifier opMethodName(List<SourceFileRange> operatorRanges, Operator op) {
		return new Identifier(operatorRanges, op.getMethodName());
	}
	Identifier opMethodName(Operator op) {
		return opMethodName(NOT_FROM_SOURCE, op);
	}
	protected DesugarResult<List<P2<Identifier, CoreExpr>>> addMethod(final SourceExpr methodSourceExpr, final SourceExpr signatureSourceExpr, final CoreExpr body, final CoreExpr postcondition, final List<P2<Identifier,CoreExpr>> methods) {

		final DesugarResult<List<P2<Identifier, CoreExpr>>> result = method(
                methodSourceExpr, signatureSourceExpr, body, postcondition
        ).mapValue(methods::cons);
		return result;
	}

	protected DesugarResult<P2<Identifier, CoreExpr>> method(
            final SourceExpr methodSourceExpr,
            final SourceExpr signatureSourceExpr,
            final CoreExpr body,
            final CoreExpr postcondition) {
	    final DesugarResult<P2<Identifier,CoreExpr>> result = signatureSourceExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<Identifier,CoreExpr>>>() {
			@Override
			public DesugarResult<P2<Identifier,CoreExpr>> binaryOp(BinaryOp targetBOp) {
				switch(targetBOp.getOperator()) {
				case MEMBER_OF: return methodWithGuarantee(targetBOp);
				case CALL: return methodWithArgs(targetBOp);
				case JUXTAPOSITION: return mixfixMethodPart(targetBOp); // a(b)c ... mixfix with no final argument list
				case PROJECTION: return methodWithSelfNameAndNoArgs(targetBOp); // self.(x) = ... or self.x = ...
				default: return fallback(targetBOp);
				}
			}


			@Override
			public DesugarResult<P2<Identifier,CoreExpr>> unaryOp(final UnaryOp targetOp) {
				switch(targetOp.getOperator()) {
				case PARENS:
					return targetOp.getOperand().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<Identifier,CoreExpr>>>() {
						@Override
						public DesugarResult<P2<Identifier,CoreExpr>> binaryOp(BinaryOp op) {
							switch(op.getOperator()) {
							case COMMA:
							case NEWLINE:
							case JUXTAPOSITION:
								return fallback(op);
							default:
							}
							boolean selfOnRight = op.getOperator().isSelfOnRightMethodOperator();
							Option<SourceExpr> selfBinding = Option.some(selfOnRight ? op.getRight(): op.getLeft());
							SourceExpr other = selfOnRight ? op.getLeft() : op.getRight();
							Identifier name = opMethodName(op.getOperator());
							return method(methodSourceExpr, op.getOperator(), other, selfBinding, body)
									.mapValue((p) -> P.p(name, (CoreExpr)p._2()));
						}

						@Override
						public DesugarResult<P2<Identifier,CoreExpr>> unaryOp(UnaryOp op) {
							switch(op.getOperator()) {
							case OBJECT_LITERAL:
							case BRACKETS:
								return fallback(op);
							default:
							}
							SourceExpr selfBinding = op.getOperand();
							return bindSelf(selfBinding, body).mapValue(bodyWithSelf -> P.p(op.getOperator().getMethodIdentifier(), bodyWithSelf));
						}
						@Override
						public DesugarResult<P2<Identifier,CoreExpr>> fallback(SourceExpr other) {
							Identifier name = new Identifier("_");
							CoreExpr body = new BadCoreExpr(other.getSourceFileRanges(), "Empty method signature");
							return withValue(P.p(name, body));
						}
					});
				default: return super.unaryOp(targetOp);
				}

			}

			private DesugarResult<P2<Identifier,CoreExpr>> methodWithSelfNameAndNoArgs(final BinaryOp signature) {
				final Identifier selfBinding = expectIdentifier(signature.getLeft());
				final Identifier name = expectIdentifier(signature.getRight());
				return method(methodSourceExpr, name, List.nil(), Option.some(selfBinding), body);
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
			private DesugarResult<P2<Identifier,CoreExpr>> mixfixMethodPart(final BinaryOp juxtaposition) {
				return juxtaposition.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<Identifier,CoreExpr>>>() {
					@Override
					public DesugarResult<P2<Identifier,CoreExpr>> fallback(SourceExpr other) {
						BadCoreExpr problem = new BadCoreExpr(juxtaposition.getSourceFileRanges(), "Invalid method signature");
						return withValue(P.p(Identifier.UNDERSCORE, problem));
					}

					@Override
					public DesugarResult<P2<Identifier,CoreExpr>> binaryOp(BinaryOp op) {
						switch(op.getOperator()) {
						case NEWLINE:
						case JUXTAPOSITION:
						case CALL: {
							Identifier newNameSuffix = expectIdentifier(juxtaposition.getRight());
							List<SourceExpr> newArgumentListsSuffix = List.nil();
							return methodWithArgs(op, Option.some(newNameSuffix), newArgumentListsSuffix, SourceExprDesugarer.this);
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
			private DesugarResult<P2<Identifier,CoreExpr>> methodWithArgs(final BinaryOp call) {
				return methodWithArgs(call, Option.none(), List.nil(), SourceExprDesugarer.this);
			}

			/**
			 * Desugar the field definition as a method that takes parameters.
			 */
			private DesugarResult<P2<Identifier,CoreExpr>> methodWithArgs(final BinaryOp call, final Option<Identifier> nameSuffix, final List<SourceExpr> argumentListsSuffix, final SourceExprDesugarer ds) {
				final SourceExpr methodLeftExpr = call.getLeft();
				final SourceExpr argsExpr = call.getRight();
				return nonNull(methodLeftExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<Identifier,CoreExpr>>>() {
					@Override
					public DesugarResult<P2<Identifier,CoreExpr>> binaryOp(BinaryOp methodDefBOp) {
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
					private DesugarResult<P2<Identifier,CoreExpr>> projection(BinaryOp methodDefBOp) {
						final SourceExpr nameSourceExpr = methodDefBOp.getRight();
						final SourceExpr selfNameSourceExpr = methodDefBOp.getLeft();
						final SourceExpr selfBinding = selfNameSourceExpr;
						return apply(nameSourceExpr, Option.some(selfBinding), ds);
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
					private DesugarResult<P2<Identifier,CoreExpr>> juxtaposition(final BinaryOp methodDefBOp) {
						// for a(b)c(d)
						// argsExpr = (d)
						// methodDefBOp.right = id(c)
						// methodDefBOp.left = call(id(a),(b))
						return methodDefBOp.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<Identifier,CoreExpr>>>() {
							@Override
							public DesugarResult<P2<Identifier,CoreExpr>> fallback(SourceExpr other) {
								BadCoreExpr problem = new BadCoreExpr(methodDefBOp.getSourceFileRanges(), "Invalid method signature");
								return ds.withValue(P.p(Identifier.UNDERSCORE, problem));
							}

							@Override
							public DesugarResult<P2<Identifier,CoreExpr>> binaryOp(BinaryOp op) {
								switch(op.getOperator()) {
								case CALL: {
									Identifier key = expectIdentifier(methodDefBOp.getRight());
									Identifier newNameSuffix = concatNameParts(key, nameSuffix);
									List<SourceExpr> newArgumentListsSuffix = argumentListsSuffix.cons(argsExpr);
									return methodWithArgs(op, Option.some(newNameSuffix), newArgumentListsSuffix, ds);
								}
								default:
									return fallback(op);
								}
							}

						});
					}


					DesugarResult<P2<Identifier,CoreExpr>> apply(SourceExpr nameExpr, Option<SourceExpr> selfBinding, SourceExprDesugarer ds) {
						final Identifier key = expectIdentifier(nameExpr);
						return ds.method(
								methodSourceExpr,
								concatNameParts(key, nameSuffix),
								flattenArgumentLists(argumentListsSuffix.cons(argsExpr)),
								selfBinding,
								body);
					}
					@Override
					public DesugarResult<P2<Identifier,CoreExpr>> fallback(SourceExpr other) {
						return apply(methodLeftExpr, Option.none(), SourceExprDesugarer.this);
					}
				}));
			}

			/**
			 * Desugar the field as a method that doesn't take any parameters
			 */
			private DesugarResult<P2<Identifier,CoreExpr>> methodWithGuarantee(BinaryOp targetBOp) {
				final SourceExpr newLvalueExpr = targetBOp.getLeft();
				final DesugarResult<CoreExpr> newGuaranteeDs = expr(targetBOp.getRight());
				final CoreExpr combinedGuarantee = composeGuarantees(postcondition, newGuaranteeDs.getValue());
				return newGuaranteeDs.method(methodSourceExpr, newLvalueExpr, body, combinedGuarantee);
			}

			/**
			 * In this case the lvalue wasn't something we recognize normally so it should be an identifier, which is the name of the
			 * method.
			 */
			@Override
			public DesugarResult<P2<Identifier,CoreExpr>> fallback(SourceExpr other) {
				final Identifier key = expectIdentifier(signatureSourceExpr);
				return withValue(P.p(key, body));
			}
		});
	    return result;
    }

	/**
	 * Desugar something the MUST be an identifier.  If it's not an identifier, a placeholder
	 * is returned - an instance of BadIdentifier - that can be used as an identifier to continue
	 * with desugaring but which will be reported as an error.
	 */
	protected static Identifier expectIdentifier(final SourceExpr sourceExpr) {
		return sourceExpr.acceptVisitor(new BaseSourceExprVisitor<Identifier>() {
			@Override
			public Identifier identifier(Identifier key) {
				return key;
			}

			@Override
			public Identifier fallback(SourceExpr other) {
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
	 * @return A new CoreExpr representing the combined guarantee
	 */
	protected CoreExpr composeGuarantees(CoreExpr guarantee, CoreExpr newGuarantee) {
		if(guarantee.equals(Identifier.TRUE)) {
			return newGuarantee;
		} else if(newGuarantee.equals(Identifier.TRUE)) {
			return guarantee;
		} else {
			return Call.binaryOp(guarantee, Operator.LOGICAL_AND, newGuarantee);
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
		final DesugarResult<List<P2<Identifier,CoreExpr>>> methodsDs = headings.zip(values).foldRight(new F2<P2<SourceExpr,SourceExpr>, DesugarResult<List<P2<Identifier,CoreExpr>>>, DesugarResult<List<P2<Identifier,CoreExpr>>>>() {
			@Override
			public DesugarResult<List<P2<Identifier,CoreExpr>>> f(
					P2<SourceExpr, SourceExpr> p,
					DesugarResult<List<P2<Identifier,CoreExpr>>> ds) {
				if(p == null || ds == null) throw new NullPointerException();
				final SourceExpr headingExpr = nonNull(p._1());
				final SourceExpr cellSourceExpr = nonNull(p._2());
				final List<P2<Identifier,CoreExpr>> tailMethods = ds.getValue();
				final DesugarResult<CoreExpr> cellDs = ds.expr(cellSourceExpr);
				return cellDs.addMethod(null, headingExpr, cellDs.getValue(), Identifier.TRUE, tailMethods);
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
		return bodyDs.functionLiteral(sourceExpr, argsSourceExpr, body);
	}

	protected DesugarResult<CoreExpr> functionLiteral(final SourceExpr sourceExpr,
			final SourceExpr argsSourceExpr, final CoreExpr body) {
		return argsSourceExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
			@Override
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				return functionLiteral(sourceExpr, argsSourceExpr, Option.none(), body);
			}

			@Override
			public DesugarResult<CoreExpr> binaryOp(BinaryOp op) {
				if(op.getOperator() == Operator.CALL) {
					final SourceExpr selfBinding = op.getLeft();
					final SourceExpr args = op.getRight();
					DesugarResult<CoreExpr> selfBoundDs = bindSelf(selfBinding, body);
					final DesugarResult<CoreExpr> result = functionLiteral(sourceExpr, args, selfBoundDs.getValue());
					return result;
				} else {
					return fallback(op);
				}
			}
		});
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
	protected DesugarResult<CoreExpr> functionLiteral(final SourceExpr methodSourceExpr, final SourceExpr args, final Option<SourceExpr> selfBinding, final CoreExpr body) {
		return args.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
			@Override
			public DesugarResult<CoreExpr> binaryOp(BinaryOp op) {
				switch(op.getOperator()) {
				case MEMBER_OF:
					// (x,y):Guarantee = ...
					return applyGuarantee(op);
				default:
					return fallback(op);
				}
			}

			private DesugarResult<CoreExpr> applyGuarantee(BinaryOp op) {
				final SourceExpr newArgs = op.getLeft();
				final SourceExpr newGuaranteeSourceExpr = op.getRight();
				final DesugarResult<CoreExpr> newGuaranteeDs = expr(newGuaranteeSourceExpr);
				CoreExpr newBody = SourceExprDesugarer.this.applyGuarantee(body, op.getOperator(), newGuaranteeDs.getValue());
				return newGuaranteeDs.functionLiteral(methodSourceExpr, newArgs, selfBinding, newBody);
			}

			@Override
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				final List<SourceExpr> exprs = flattenCommas(stripParens(args));
				return method(methodSourceExpr, Identifier.UNDERSCORE, single(exprs), selfBinding, body).mapValue(P2.__2());
			}
		});

	}

	/**
	 * Take a list of SourceExpr that might be a comma-separated list of arguments and make that into
	 * a list of arguments lists with the arguments split by commas.
	 */
	List<List<SourceExpr>> flattenArgumentLists(List<SourceExpr> a) {
		return a.map(x -> flattenCommas(x));
	}

	public DesugarResult<P2<Identifier,CoreExpr>> method(SourceExpr methodSourceExpr, Operator operator, SourceExpr other, Option<SourceExpr> selfBinding, CoreExpr body) {
		return functionLiteral(methodSourceExpr, other, selfBinding, body).mapValue(func -> P.p(operator.getMethodIdentifier(), func));
	}

	protected DesugarResult<P2<Identifier,CoreExpr>> method(SourceExpr methodSourceExpr, final Identifier methodName, List<List<SourceExpr>> argumentListsSourceExprs, Option<SourceExpr> selfBinding, CoreExpr body) {
		return method(methodSourceExpr, methodName, argumentListsSourceExprs, selfBinding, Identifier.TRUE, body, Identifier.TRUE);
	}

	protected DesugarResult<P2<Identifier, CoreExpr>> method(final SourceExpr methodSourceExpr, final Identifier methodName, List<List<SourceExpr>> argumentListsSourceExprs, Option<SourceExpr> selfBinding, CoreExpr precondition, CoreExpr currBody, CoreExpr postcondition) {
		final DesugarResult<CoreExpr> funcDs = argumentListsSourceExprs.zipIndex().foldRight(
			(P2<List<SourceExpr>, Integer> a, DesugarResult<CoreExpr> argumentListsDsState) -> {
				List<SourceExpr> argsListSourceExprs = a._1();
				final int argListNumber = a._2();
				CoreExpr tempBody = argumentListsDsState.getValue();
				DesugarResult<P3<CoreExpr,CoreExpr,List<Identifier>>> argListDs = argsListSourceExprs.zipIndex().foldRight(
					(P2<SourceExpr,Integer> argExprWithIndex,
						DesugarResult<P3<CoreExpr,CoreExpr,List<Identifier>>> argListDsState) -> {
						final int argNumber = argExprWithIndex._2();
						final SourceExpr argExpr = argExprWithIndex._1();
						final int index = argExprWithIndex._2();

						// Need another fold because there are multiple parameter lists.

						CoreExpr _precondition = argListDsState.getValue()._1();
						CoreExpr body = argListDsState.getValue()._2();
						List<Identifier> paramList = argListDsState.getValue()._3();
						final DesugarResult<P3<CoreExpr, CoreExpr, List<Identifier>>> paramDs = argListDsState.methodFormalArgument(
								methodSourceExpr,
								paramList,
								argExpr,
								_precondition,
								body,
								index,
								argListNumber
						);
						return paramDs;
				}, argumentListsDsState.withValue(P.p(Identifier.TRUE, tempBody, List.nil())));

				// TODO Precondition is not being used here ...
				CoreExpr newPrecondition = argListDs.getValue()._1();
				CoreExpr newBody = argListDs.getValue()._2();
				final List<Identifier> args = argListDs.getValue()._3();
				FunctionLiteral func = new FunctionLiteral(args, newBody);
				return argListDs.withValue((CoreExpr)func);
		}, this.withValue(currBody));

		final Option<DesugarResult<CoreExpr>> temp = selfBinding.map(
				(name) -> funcDs.bindSelf(name, funcDs.getValue())
		);
		DesugarResult<CoreExpr> funcWithSelfNameDs = temp.orSome(funcDs);

		return funcWithSelfNameDs.mapValue((slotValue) -> P.p(methodName, slotValue));
	}

	/**
	 * Desugaring a parameter declaration in a method's formal parameter list.
	 * @param paramList List of formal parameters; this is mutated to add the parameter(s) to the end
	 * @param argExpr The source expression for the formal argument declaration
	 * @param bodyDs The desugared function body - this might be wrapped in another function while unpacking parameters
	 * @param index TODO
	 * @return The result of the desugaring includes the new source mappings and the new method body
	 */
	public DesugarResult<P3<CoreExpr, CoreExpr, List<Identifier>>> methodFormalArgument(final SourceExpr methodSourceExpr, final List<Identifier> paramList, final SourceExpr argExpr, final CoreExpr precondition, final CoreExpr body, final int index, final int index2) {
		final SourceExprDesugarer ds = SourceExprDesugarer.this;
		return nonNull(argExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P3<CoreExpr, CoreExpr, List<Identifier>>>>() {

			@Override
			public DesugarResult<P3<CoreExpr, CoreExpr, List<Identifier>>> binaryOp(final BinaryOp argOp) {
				switch(argOp.getOperator()) {
				case MEMBER_OF:
				case EQ:
				case GE:
				case GT:
				case LE:
				case LT:
				case NEQ:
					final Identifier paramDecl = expectIdentifier(argOp.getLeft());
					final DesugarResult<CoreExpr> assertionDs = expr(argOp.getRight());
					// Check parameter precondition
					CoreExpr newPrecondition = insertContract(precondition, paramDecl, argOp.getOperator(), assertionDs.getValue());
					return assertionDs.withValue(P.p(newPrecondition, body, cons(paramDecl, paramList)));

				default:
					return fallback(argOp);
				}
			}

			@Override
			public DesugarResult<P3<CoreExpr, CoreExpr, List<Identifier>>> identifier(Identifier key) {
				return ds.withValue(P.p(precondition, body, cons(key, paramList)));
			}
			@Override
			public DesugarResult<P3<CoreExpr, CoreExpr, List<Identifier>>> fallback(SourceExpr other) {
				Identifier tempArgName = new Identifier("__"+index);
				DesugarResult<List<P2<Identifier, CoreExpr>>> pairs = localVariableDef(other, tempArgName);
				Let let = new Let(pairs.getValue(), body);
				return pairs.withValue(P.p(precondition, let, paramList.cons(tempArgName)));
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
	protected CoreExpr insertContract(CoreExpr currentPrecondition, Identifier parameterName,
			Operator operator, CoreExpr constraint) {
		Call check = new Call(constraint.getSourceFileRanges(), new SlotReference(parameterName, opMethodName(operator)), List.single(constraint));
		return composePreconditions(currentPrecondition, check);
	}

	private CoreExpr composePreconditions(CoreExpr a, CoreExpr b) {
		if(a.equals(Identifier.TRUE)) {
			return b;
		} else if(b.equals(Identifier.TRUE)){
			return a;
		} else {
			return Call.binaryOp(a, Operator.LOGICAL_AND, b);
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
				if(op.getOperator() == sep || op.getOperator() == Operator.NEWLINE) {

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
	protected DesugarResult<CoreExpr> binaryOpToCall(final BinaryOp op, boolean optional) {
		final SourceExpr leftSourceExpr = op.getLeft();
		final Operator operator = op.getOperator();
		List<SourceFileRange> operatorRanges = op.getOperatorRanges();
		final SourceExpr rightSourceExpr = op.getRight();
		return binaryOpToCall(op, leftSourceExpr, operator, operatorRanges, rightSourceExpr, optional);
	}

	protected DesugarResult<CoreExpr> binaryOpToCall(SourceExpr op, final SourceExpr leftSourceExpr, final Operator operator, List<SourceFileRange> operatorRanges, final SourceExpr rightSourceExpr, boolean optional) {
		final DesugarResult<CoreExpr> leftDs = expr(leftSourceExpr);
		final DesugarResult<CoreExpr> rightDs = leftDs.expr(rightSourceExpr);
		return rightDs.withDesugared(op, binaryOpToCall(op.getSourceFileRanges(), leftDs.getValue(), operator, operatorRanges, rightDs.getValue(), optional));
	}

	protected Call binaryOpToCall(List<SourceFileRange> ranges, final CoreExpr leftCoreExpr, final Operator operator,	List<SourceFileRange> operatorRanges, final CoreExpr rightCoreExpr, boolean optional) {
		final boolean rightAssoc = operator.isRightAssociative();
		final CoreExpr target = rightAssoc?rightCoreExpr:leftCoreExpr;
		final CoreExpr parameter = rightAssoc?leftCoreExpr:rightCoreExpr;
		final CoreExpr callTarget = operator == Operator.CALL? target : new SlotReference(target, opMethodName(operatorRanges, operator));
		return new Call(ranges, callTarget, List.single(parameter));
	}


	protected DesugarResult<CoreExpr> nullaryFunctionLiteral(SourceExpr sourceExpr, SourceExpr body) {
		final DesugarResult<CoreExpr> bodyDs = expr(body);
		return bodyDs.functionLiteral(sourceExpr, EmptyExpr.SYNTHETIC_INSTANCE, bodyDs.getValue());
	}

	protected DesugarResult<CoreExpr> singletonListLiteral(UnaryOp op) {
		final SourceExpr operandSourceExpr = op.getOperand();
		final DesugarResult<CoreExpr> operandDs = expr(operandSourceExpr);
		return operandDs.withDesugared(op, new ListLiteral(op.getSourceFileRanges(), single(operandDs.getValue())));
	}

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
				return binaryOpToCall(op, false);

			case EXTEND:
				return extend(op);

			case JUXTAPOSITION:
			case NEWLINE:
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
			case NULLARY_FUNCTION_LITERAL: return nullaryFunctionLiteral(op, op.getOperand());
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
				return unaryOpToSlotReference(op);

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
	}

	public DesugarResult<CoreExpr> inspect(UnaryOp op) {
		final DesugarResult<CoreExpr> exprDs = expr(op.getOperand());
		return exprDs.withDesugared(op, new Inspect(op.getSourceFileRanges(), exprDs.getValue()));
	}

	public DesugarResult<CoreExpr> selector(UnaryOp op) {
		final Identifier __tmp = Identifier.__TMP;
		P3<CoreExpr,CoreExpr,SourceExprDesugarer> ps = op.getOperand().acceptVisitor(new BaseSourceExprVisitor<P3<CoreExpr,CoreExpr,SourceExprDesugarer>>() {
			@Override
			public P3<CoreExpr,CoreExpr,SourceExprDesugarer> identifier(Identifier field) {
				DesugarResult<CoreExpr> preDs = projection(op, (CoreExpr)__tmp, field);
				CoreExpr precondition = preDs.getValue();
				DesugarResult<CoreExpr> bodyDs = preDs.projection(op, (CoreExpr)__tmp, field);
				CoreExpr body = bodyDs.getValue();
				return P.p(precondition, body, bodyDs);
			}

			@Override
			public P3<CoreExpr,CoreExpr,SourceExprDesugarer> binaryOp(BinaryOp op) {
				// Variant has arguments
				DesugarResult<CoreExpr> preDs = desugar(new BinaryOp(op.getOperator(), op.getOperatorRanges(), new BinaryOp(Operator.OPT_PROJECTION, op.getOperatorRanges(), __tmp, op.getLeft()), op.getRight()));
				CoreExpr precondition = preDs.getValue();
				DesugarResult<CoreExpr> bodyDs = preDs.desugar(new BinaryOp(op.getOperator(), op.getOperatorRanges(), new BinaryOp(Operator.PROJECTION, op.getOperatorRanges(), __tmp, op.getLeft()), op.getRight()));
				CoreExpr body = bodyDs.getValue();
				return P.p(precondition, body, bodyDs);
			}

			@Override
			public P3<CoreExpr, CoreExpr, SourceExprDesugarer> unaryOp(UnaryOp op) {
				// Callback shorthand - .(x) == (f) -> f(x)
				if(op.getOperator() == Operator.PARENS) {
					CoreExpr precondition = Identifier.TRUE; // TODO
					DesugarResult<CoreExpr> bodyDs = call(op, __tmp, op.getOperand(), flattenCommas(op.getOperand()));
					return P.p(precondition, bodyDs.getValue(), bodyDs);
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
		// TODO precondition!
		return ds.withDesugared(op, new FunctionLiteral(single(__tmp), body));
	}

	public DesugarResult<CoreExpr> extend(BinaryOp op) {
		final DesugarResult<CoreExpr> leftDs = expr(op.getLeft());
		final DesugarResult<CoreExpr> rightDs = leftDs.expr(op.getRight());
		return rightDs.withDesugared(op, new Extend(op.getSourceFileRanges(), leftDs.getValue(), rightDs.getValue()));
	}

	public DesugarResult<CoreExpr> let(final BinaryOp op) {
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

		final DesugarResult<CoreExpr> bodyDs = desugar(op.getRight());
		return bodyDs.let(op, namedValues, bodyDs.getValue());

	}

	public DesugarResult<CoreExpr> let(final BinaryOp op, List<SourceExpr> namedValues, CoreExpr body) {
	    final DesugarResult<List<P2<Identifier, CoreExpr>>> bindingsDs = namedValues.foldRight((a, ds) -> {
	    	List<P2<Identifier,CoreExpr>> bindings = ds.getValue();
	    	DesugarResult<List<P2<Identifier, CoreExpr>>> bindingDs = a.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<P2<Identifier, CoreExpr>>>>() {
    			@Override
    			public DesugarResult<List<P2<Identifier, CoreExpr>>> binaryOp(BinaryOp letOp) {
    				if(letOp.getOperator() == Operator.ASSIGNMENT) {
    					return ds.assignment(letOp, letOp.getLeft(), letOp.getRight());
    				}
    				return fallback(letOp);
    			}

    			@Override
    			public DesugarResult<List<P2<Identifier, CoreExpr>>> fallback(SourceExpr other) {
    				//BinaryOp letOp = new BinaryOp(other.getSourceFileRanges(), Operator.ASSIGNMENT, SourceFileRange.EMPTY_LIST, Identifier.UNDERSCORE, other);
    				return ds.localVariableDef(Identifier.UNDERSCORE, other);
    			}
    		});
	    	return bindingDs.withValue(bindings.append(bindingDs.getValue()));
	    }, withValue(List.<P2<Identifier,CoreExpr>>nil()));

		final List<P2<Identifier, CoreExpr>> bindings = bindingsDs.getValue();
		return bindingsDs.withDesugared(op, new Let(op.getSourceFileRanges(), bindings, body));
    }

	public DesugarResult<List<P2<Identifier, CoreExpr>>> assignment(SourceExpr letOp, SourceExpr left, SourceExpr right) {
		return left.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<P2<Identifier, CoreExpr>>>>() {
			@Override
			public DesugarResult<List<P2<Identifier, CoreExpr>>> binaryOp(BinaryOp op) {
				switch(op.getOperator()) {
				case JUXTAPOSITION:
				case CALL: {
					return localFunctionDef(letOp, op.getLeft(), op.getRight(), right).mapValue(List::single);
				}
				default:
					return fallback(op);
				}
			}

			@Override
			public DesugarResult<List<P2<Identifier, CoreExpr>>> fallback(SourceExpr argPattern) {
				return localVariableDef(left, right);
			}

		});
	}

	public DesugarResult<List<P2<Identifier, CoreExpr>>> localVariableDef(
			final SourceExpr lhs,
			final SourceExpr value) {
		// x = y, z == (x -> z)(y) == {(x) = z}(y)
		return lhs.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<P2<Identifier, CoreExpr>>>>() {
			@Override
			public DesugarResult<List<P2<Identifier, CoreExpr>>> unaryOp(UnaryOp op) {
				switch(op.getOperator()) {
				case OBJECT_LITERAL:
					return unpackObject(op);
				default:
				    return fallback(op);
				}
			}
			private DesugarResult<List<P2<Identifier, CoreExpr>>> unpackObject(UnaryOp op) {
				List<SourceExpr> slotBindings = flattenCommas(op.getOperand());
				return slotBindings.foldRight(
					this::unpackSlot,
					withValue(List.<P2<Identifier, CoreExpr>>nil())
				);
            }
			private DesugarResult<List<P2<Identifier, CoreExpr>>> unpackSlot(SourceExpr slot, DesugarResult<List<P2<Identifier, CoreExpr>>> ds) {
				return slot.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<P2<Identifier, CoreExpr>>>>() {
					@Override
					public DesugarResult<List<P2<Identifier, CoreExpr>>> identifier(Identifier identifier) {
						BinaryOp projection = new BinaryOp(Operator.PROJECTION, value, identifier);
						DesugarResult<CoreExpr> projectionDs = expr(projection);
						P2<Identifier, CoreExpr> pair = P.p(identifier, projectionDs.getValue());
					    return projectionDs.withValue(ds.getValue().cons(pair));
					}

					@Override
					public DesugarResult<List<P2<Identifier, CoreExpr>>> binaryOp(BinaryOp op) {
						switch(op.getOperator()) {
						case ASSIGNMENT:
							return ds.localVariableDef(op.getRight(), new BinaryOp(Operator.PROJECTION, value, op.getLeft())).mapValue(vars -> vars.append(ds.getValue()));
						default:
						    return super.binaryOp(op);
						}
					}

					@Override
					public DesugarResult<List<P2<Identifier, CoreExpr>>> fallback(SourceExpr other) {
					    return withValue(List.single(P.p(Identifier.UNDERSCORE, new BadCoreExpr(other.getSourceFileRanges(), "Expected identifier / lvalue"))));
					}
				});
			}
			@Override
			public DesugarResult<List<P2<Identifier, CoreExpr>>> identifier(Identifier name) {
				final DesugarResult<CoreExpr> valueDs = expr(value);
				return valueDs.withValue(List.single(P.p(name, valueDs.getValue())));
			}

			@Override
			public DesugarResult<List<P2<Identifier, CoreExpr>>> fallback(SourceExpr other) {
			    return withValue(List.single(P.p(Identifier.UNDERSCORE, new BadCoreExpr(other.getSourceFileRanges(), "Expected identifier / lvalue"))));
			}
		});
	}

	public DesugarResult<P2<Identifier, CoreExpr>> localFunctionDef(SourceExpr letOp, SourceExpr name, SourceExpr args, SourceExpr body) {
		DesugarResult<CoreExpr> bodyDs = expr(body);
		return bodyDs.localFunctionDef(letOp, name, args, bodyDs.getValue());
	}

	public DesugarResult<P2<Identifier, CoreExpr>> localFunctionDef(SourceExpr letOp, SourceExpr name, SourceExpr args, CoreExpr body) {
		DesugarResult<CoreExpr> valueDs = functionLiteral(letOp, args, body);
		CoreExpr func = valueDs.getValue();
		return localFunctionDef(letOp, name, func);
	}

	protected DesugarResult<P2<Identifier, CoreExpr>> localFunctionDef(SourceExpr letOp, SourceExpr name, CoreExpr func) {
	    return name.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<Identifier, CoreExpr>>>() {
			@Override
			public DesugarResult<P2<Identifier, CoreExpr>> binaryOp(BinaryOp op) {
				if(op.getOperator() == Operator.JUXTAPOSITION) {
					Identifier rightName = expectIdentifier(op.getRight());
					return op.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<Identifier, CoreExpr>>>() {
						@Override
						public DesugarResult<P2<Identifier, CoreExpr>> binaryOp(BinaryOp leftOp) {
							if(leftOp.getOperator() == Operator.CALL) {
								return localFunctionDef(letOp, leftOp.getLeft(), leftOp.getRight(), func)
										.mapValue(p -> p.map1(leftName -> concatNameParts(leftName, rightName)));
							}
							return fallback(leftOp);
						}

						@Override
						public DesugarResult<P2<Identifier, CoreExpr>> fallback(SourceExpr other) {
						    return withValue(P.p(Identifier.UNDERSCORE, new BadCoreExpr(other.getSourceFileRanges(), "Expected function signature part")));
						}
					});
				}
			    return fallback(op);
			}
			@Override
			public DesugarResult<P2<Identifier, CoreExpr>> identifier(
			        Identifier identifier) {
				return withValue(P.p(identifier, func));
			}

			public SourceExprDesugarer.DesugarResult<fj.P2<Identifier,CoreExpr>> fallback(SourceExpr other) {
				return identifier(expectIdentifier(other));
			}
		});
    }
	private DesugarResult<CoreExpr> juxtaposition(final BinaryOp op) {
		return op.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
			private DesugarResult<CoreExpr> nonCallJuxtaposition() {
				return SourceExprDesugarer.this.call(op);
			}

			@Override
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				return nonCallJuxtaposition();
			}
			@Override
			public DesugarResult<CoreExpr> binaryOp(final BinaryOp opLeft) {
				if(opLeft.getOperator() == Operator.CALL) {
					return op.getRight().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
						@Override
						public DesugarResult<CoreExpr> fallback(SourceExpr other) {
							return nonCallJuxtaposition();
						}

						public SourceExprDesugarer.DesugarResult<CoreExpr> identifier(Identifier keyOnRight) {
							Operator leftOp = opLeft.getOperator();
							boolean callNext = leftOp == Operator.CALL_NEXT_METHOD || leftOp == Operator.OPT_CALL_NEXT_METHOD;
							boolean optional = leftOp == Operator.OPT_PROJECTION || leftOp == Operator.OPT_CALL_NEXT_METHOD;
							assert callNext == false;
							assert optional == false;
							//return call(opLeft, keyOnRight, List.nil());
							DesugarResult<CoreExpr> leftCall = call(opLeft);
							return leftCall.mapValue(call -> addNamePartToCall(call, keyOnRight));
						}

						private CoreExpr addNamePartToCall(CoreExpr value, Identifier keyOnRight) {
	                        return value.acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {
	                        	@Override
	                        	public CoreExpr call(Call n) {
	                        		CoreExpr newTarget = addNamePartToCall(n.target, keyOnRight);
	                        		if(newTarget != n.target)
	                        			return new Call(n.getSourceFileRanges(), newTarget, n.args);
	                        	    return super.call(n);
	                        	}

	                        	@Override
	                        	public CoreExpr identifier(Identifier n) {
	                        	    return concatNameParts(n, keyOnRight);
	                        	}

	                        	@Override
	                        	public CoreExpr slotReference(SlotReference n) {
	                        	    return new SlotReference(n.getSourceFileRanges(), n.object, concatNameParts(n.slotName, keyOnRight));
	                        	}

	                        	@Override
	                        	public CoreExpr fallback() {
	                        	    return new BadCoreExpr(keyOnRight.getSourceFileRanges(), "Dangling function name part %s", keyOnRight.id);
	                        	}
							});
                        }
					});
				} else {
					return nonCallJuxtaposition();
				}
			}
		});
	}


	private DesugarResult<CoreExpr> unaryOpToSlotReference(final UnaryOp op) {
	    final DesugarResult<CoreExpr> operandCoreExpr = expr(op.getOperand());
	    return operandCoreExpr.withDesugared(op, new SlotReference(op.getSourceFileRanges(), operandCoreExpr.getValue(), opMethodName(op.getOperator())));
    }

	/**
	 * Bind to self - basically <code>(lhs = __self) => body</code>
	 */
	protected DesugarResult<CoreExpr> bindSelf(SourceExpr selfBinding, final CoreExpr body) {
		return localVariableDef(selfBinding, Identifier.__SELF).mapValue(pairs -> new Let(pairs, body));
    }


}
