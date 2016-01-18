package banjo.expr.core;

import static fj.data.List.cons;
import static fj.data.List.single;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Objects;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import banjo.expr.BadExpr;
import banjo.expr.source.BadSourceExpr;
import banjo.expr.source.BaseSourceExprVisitor;
import banjo.expr.source.BinaryOp;
import banjo.expr.source.EmptyExpr;
import banjo.expr.source.Operator;
import banjo.expr.source.SourceExpr;
import banjo.expr.source.SourceExprFactory;
import banjo.expr.source.SourceExprVisitor;
import banjo.expr.source.UnaryOp;
import banjo.expr.token.BadIdentifier;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.token.OperatorRef;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.FileRange;
import banjo.expr.util.ParserReader;
import banjo.expr.util.SourceFileRange;
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

public class CoreExprFactory implements SourceExprVisitor<CoreExprFactory.DesugarResult<CoreExpr>>{
	protected static final Set<SourceFileRange> NOT_FROM_SOURCE = SourceFileRange.EMPTY_SET;
	static final Set<String> EMPTY_STRING_SET = Set.empty(Ord.stringOrd);

	public static class DesugarResult<T> extends CoreExprFactory {
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
	static final fj.data.Set<SourceExpr> EMPTY_SOURCE_EXPR_SET = fj.data.Set.empty(SourceExpr.sourceExprOrd);
    public static final CoreExprFactory INSTANCE = new CoreExprFactory();
    public static final String LIB_PATH_SYS_PROPERTY = "banjo.path";

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
		return Objects.requireNonNull(sourceExpr.acceptVisitor(this));
	}

	/**
	 * Desugar a few exprs at once.  The state is accumulated from one to the next so that the
	 * last element of the array has the accumulated cache from the previous desugarings.
	 */
	public DesugarResult<CoreExpr[]> exprs(SourceExpr parent, SourceExpr ... sourceExprs) {
		final CoreExpr[] results = new CoreExpr[sourceExprs.length];
		CoreExprFactory desugarer = this;
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
	 * @param base TODO
	 * @param sourceOffset Source offset to the start of op
	 */
	protected DesugarResult<CoreExpr> projection(BinaryOp op, boolean base) {
		return projection(op, op.getLeft(), op.getRight(), base);
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
	    return projection(sourceExpr, objectExpr, projectionExpr, false);
	}

	/**
	 * Desugar a projection.  The most basic form of projection is <code>foo.bar</code> which desugars
	 * the property <code>bar</code> in object <code>foo</code>, translating into a call <code>foo.bar()</code>.
	 *
	 * More complex projections are possible also, however.  <code>foo.{bar,baz}</code> will translate into
	 * <code>{bar = foo.bar, baz = foo.baz}</code> and <code>foo.[bar, baz]</code> will translate into
	 * <code>[foo.bar, foo.baz]</code>.  Renames are possible when selecting fields for a new object,
	 * so <code>foo.{bar = baz, baz = bar}</code> would swap the fields as if <code>{bar = foo.baz, baz = foo.bar}</code>
	 * were written.
	 *
	 * @param sourceExpr Root source expression for the projection.
	 * @param objectExpr Left-hand side of the projection; the "base" object we're projecting from
	 * @param projectionExpr Right-hand side of the projection
	 * @param base TODO
	 */
	protected DesugarResult<CoreExpr> projection(final SourceExpr sourceExpr, final SourceExpr objectExpr, final SourceExpr projectionExpr, boolean base) {
		return projectionExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
			@Override
			public DesugarResult<CoreExpr> identifier(Identifier id) {
				final DesugarResult<CoreExpr> objectDs = expr(objectExpr);
				return objectDs.slotReference(sourceExpr, objectDs.getValue(), id, base);
			}

			@Override
			public DesugarResult<CoreExpr> unaryOp(UnaryOp op) {
				if(op.getOperator() == Operator.OBJECT_LITERAL) {
					return objectLiteralProjection(sourceExpr, objectExpr, flattenCommas(op.getOperand()));
				}
			    return super.unaryOp(op);
			}
			@Override
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				return withDesugared(sourceExpr, new BadCoreExpr(sourceExpr.getSourceFileRanges(), "Expected identifier after '.'"));
			}

		});
	}

	/**
	 * Desugaring like:
	 *
	 * <ul>
	 * <li><code>a.{b} == {b = a.b}</code></li>
	 * <li><code>a.{b, c} == {b = a.b, c=a.c}</code></li>
	 * <li><code>a.{b=c} == {b = a.c}</code></li>
	 * <li><code>a.{b=c, d=c} == {b = a.c, d = a.c}</code></li>
	 * </ul>
	 *
	 */
	protected DesugarResult<CoreExpr> objectLiteralProjection(SourceExpr sourceExpr, SourceExpr objectExpr, List<SourceExpr> projections) {
		final BaseSourceExprVisitor<SourceExpr> fieldVisitor = new BaseSourceExprVisitor<SourceExpr>() {
			@Override
			public SourceExpr fallback(SourceExpr other) {
			    return new BadSourceExpr.InvalidProjection(other.getSourceFileRanges(), "Expected projection like x or x = y");
			}

			@Override
			public SourceExpr identifier(Identifier identifier) {
				// Translate `obj.{x}` to `{x = obj.x}`
				return mapping(identifier, identifier, identifier);
			}

			@Override
			public SourceExpr binaryOp(BinaryOp op) {
				if(op.getOperator() == Operator.ASSIGNMENT) {
					// Translate `obj.{x = y}` to `{x = obj.y}`
					return mapping(op, op.getLeft(), op.getRight());
				} else {
				    return new BadSourceExpr.InvalidProjection(op.getSourceFileRanges(), "Expected assignment");
				}
			}

			private SourceExpr mapping(SourceExpr op,
                    final SourceExpr left, final SourceExpr right) {
				SourceExpr rightSide = insertProjectionTargetOnLeft(objectExpr,
                        right, Operator.PROJECTION);
	            return new BinaryOp(op.getSourceFileRanges(), Operator.ASSIGNMENT, SourceFileRange.EMPTY_SET, left, rightSide);
            }
		};
		return objectLiteral(sourceExpr, projections.map(p -> p.acceptVisitor(fieldVisitor)));
    }

	protected DesugarResult<CoreExpr> slotReference(final SourceExpr sourceExpr,
            final CoreExpr targetObjectExpr, Identifier slotName, boolean base) {
        return withDesugared(sourceExpr, new Projection(sourceExpr.getSourceFileRanges(), targetObjectExpr, slotName, base));
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
		final Identifier argName = new Identifier("__tmp");
		final DesugarResult<CoreExpr> projectionDs = projection(op, argName, projection, false);
		final DesugarResult<CoreExpr> projectFuncDs = projectionDs.function(op, argName, projectionDs.getValue());
		final DesugarResult<CoreExpr> mapCallDs = projectFuncDs.withDesugared(op, Call.slot(target, Operator.FUNCTION_COMPOSITION_LEFT.methodNameKey, projectFuncDs.getValue()));
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
		return new Identifier(prefix.getSourceFileRanges().union(suffix.getSourceFileRanges()), prefix.indentColumn, (prefix.id + " _ " + suffix.id).trim());
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
		return elements(sourceExpr, list, requireBullet, (ds) -> ds.withDesugared(sourceExpr, new ListLiteral(sourceExpr.getSourceFileRanges(), ds.getValue())));
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
		final DesugarResult<P2<List<SourceExpr>, List<CoreExpr>>> rowsDs = list.foldRight((e, ds) -> {
				final List<SourceExpr> unprocessedRows = ds.getValue()._1();
				final List<CoreExpr> processedRows = Objects.requireNonNull(ds.getValue()._2());
				return e.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<List<SourceExpr>, List<CoreExpr>>>>() {
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
				});
		}, this.withValue(P.p(List.nil(), List.nil())));

		// Now the remaining "unprocessed rows" are the ones with no table header, so handle those ones next
		final List<SourceExpr> unprocessedElts = rowsDs.getValue()._1();
		final DesugarResult<List<CoreExpr>> eltsDs = unprocessedElts.foldRight((e, ds) ->
				e.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<CoreExpr>>>() {
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
				}),
		rowsDs.withValue(Objects.requireNonNull(rowsDs.getValue()._2())));
		return cb.f(eltsDs);
	}

	private DesugarResult<CoreExpr> objectLiteral(SourceExpr sourceExpr, SourceExpr methodExprs) {
		final List<SourceExpr> fieldSourceExprs = flattenCommas(methodExprs);
		return objectLiteral(sourceExpr, fieldSourceExprs);
	}
	private DesugarResult<CoreExpr> objectLiteral(SourceExpr sourceExpr, final List<SourceExpr> methodSourceExprs) {
		final DesugarResult<List<Slot>> methodsDs = methodSourceExprs.foldRight(new F2<SourceExpr,DesugarResult<List<Slot>>,DesugarResult<List<Slot>>>() {
			@Override
			public DesugarResult<List<Slot>> f(SourceExpr methodSourceExpr, DesugarResult<List<Slot>> ds) {
				return ds.addMethod(methodSourceExpr, List.nil(), ds.getValue());
			}
		}, this.withValue(List.nil()));
		return methodsDs.withDesugared(sourceExpr, new ObjectLiteral(sourceExpr.getSourceFileRanges(), methodsDs.getValue()));
	}

	
	protected DesugarResult<List<Slot>> addMethod(final SourceExpr fieldSourceExpr, final List<SourceExpr> headings, final List<Slot> slots) {
		return Objects.requireNonNull(fieldSourceExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<List<Slot>>>() {
			@Override
			public DesugarResult<List<Slot>> binaryOp(BinaryOp op) {
				switch(op.getOperator()) {
				case ASSIGNMENT: return visitPair(op, Operator.ASSIGNMENT);
				case EXTEND_METHOD: return visitPair(op, Operator.EXTENSION);
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

			private DesugarResult<List<Slot>> visitPair(BinaryOp fieldOp, Operator combiningOp) {
				final SourceExpr left = fieldOp.getLeft();
				final SourceExpr right = fieldOp.getRight();
				return pair(left, right, combiningOp);
			}

			@Override
			public DesugarResult<List<Slot>> unaryOp(UnaryOp op) {
				switch(op.getOperator()) {
				case TABLE_HEADER: return visitTableHeader(op);
				default: return fallback(op);
				}
			}

			private DesugarResult<List<Slot>> visitTableHeader(UnaryOp headerOp) {
				throw new Error("Headings not supported ...");
			}

			@Override
			public DesugarResult<List<Slot>> identifier(Identifier id) {
				return pair(id, id, Operator.ASSIGNMENT);
			}

			private DesugarResult<List<Slot>> pair(SourceExpr lvalueExpr, SourceExpr valueSourceExpr, Operator combiningOp) {
				final DesugarResult<CoreExpr> eltDs = element(valueSourceExpr, headings);
				return eltDs.addMethod(fieldSourceExpr, lvalueExpr, eltDs.getValue(), Identifier.TRUE, slots, combiningOp);
			}


			@Override
			public DesugarResult<List<Slot>> fallback(SourceExpr other) {
				return addMethod(fieldSourceExpr, new Identifier(other.getSourceFileRanges(), 0, other.toSource()), new BadCoreExpr(other.getSourceFileRanges(), "Expected method definition"), Identifier.TRUE, slots, null);
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

	Identifier opMethodName(Set<SourceFileRange> operatorRanges, Operator op) {
		return new Identifier(operatorRanges, 0, op.getMethodName());
	}
	Identifier opMethodName(Operator op) {
		return opMethodName(NOT_FROM_SOURCE, op);
	}

	protected Slot applyCombiningOp(Slot slot, Operator combiningOp) {
		if(combiningOp == null || combiningOp == Operator.ASSIGNMENT)
			return slot;
		Identifier selfBinding = slot.sourceObjectBinding.orSome(Identifier.__TMP);
		final CoreExpr base = Projection.baseSlot(selfBinding, slot.name);
		CoreExpr newValue =
				combiningOp == Operator.EXTENSION ?
				new Extend(base, slot.value) :
				Call.binaryOp(base, combiningOp, slot.value);
		return new Slot(slot.name, Option.some(selfBinding), newValue);
	}

	public DesugarResult<List<Slot>> addMethod(final SourceExpr methodSourceExpr, final SourceExpr signatureSourceExpr, final CoreExpr body, final CoreExpr postcondition, final List<Slot> methods, Operator combiningOp) {
		final DesugarResult<List<Slot>> result = method(
                methodSourceExpr, signatureSourceExpr, body, postcondition
        ).mapValue(slot -> applyCombiningOp(slot, combiningOp)).mapValue(methods::cons);
		return result;
	}

	protected DesugarResult<Slot> method(
            final SourceExpr methodSourceExpr,
            final SourceExpr signatureSourceExpr,
            final CoreExpr body,
            final CoreExpr postcondition) {
	    final DesugarResult<Slot> result = signatureSourceExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<Slot>>() {
			@Override
			public DesugarResult<Slot> binaryOp(BinaryOp targetBOp) {
				switch(targetBOp.getOperator()) {
				case MEMBER_OF: return methodWithGuarantee(targetBOp);
				case CALL: return methodWithArgs(targetBOp);
				case JUXTAPOSITION: return mixfixMethodPart(targetBOp); // a(b)c ... mixfix with no final argument list
				case PROJECTION: return methodWithSelfNameAndNoArgs(targetBOp); // self.(x) = ... or self.x = ...
				default: return fallback(targetBOp);
				}
			}


			@Override
			public DesugarResult<Slot> unaryOp(final UnaryOp targetOp) {
				switch(targetOp.getOperator()) {
				case PARENS:
					return targetOp.getOperand().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<Slot>>() {
						@Override
						public DesugarResult<Slot> binaryOp(BinaryOp op) {
							Operator operator = op.getOperator();
							switch(operator.getOperatorType()) {
							case BUILTIN:
							case FUNCTION:
							case FUNCTION_SWITCHED:
								return fallback(op);
							default:
							}
							
							boolean selfOnRight = operator.isSelfOnRightMethodOperator();
							Option<SourceExpr> selfBinding = Option.some(selfOnRight ? op.getRight(): op.getLeft());
							SourceExpr other = selfOnRight ? op.getLeft() : op.getRight();
							Identifier name = opMethodName(operator);
							return method(methodSourceExpr, operator, other, selfBinding, body)
									.mapValue(s -> s.withName(name));
						}

						@Override
						public DesugarResult<Slot> unaryOp(UnaryOp op) {
							switch(op.getOperator()) {
							case OBJECT_LITERAL:
							case LIST_LITERAL:
								return fallback(op);
							default:
							}
							SourceExpr selfBinding = op.getOperand();
							return bindSelf(selfBinding, body).mapValue(p -> new Slot(op.getOperator().getMethodIdentifier(), p._1(), p._2()));
						}
						@Override
						public DesugarResult<Slot> fallback(SourceExpr other) {
							Identifier name = new Identifier("_");
							CoreExpr body = new BadCoreExpr(other.getSourceFileRanges(), "Empty method signature");
							return withValue(new Slot(name, Option.none(), body));
						}
					});
				default: return super.unaryOp(targetOp);
				}

			}

			private DesugarResult<Slot> methodWithSelfNameAndNoArgs(final BinaryOp signature) {
				final Option<SourceExpr> selfBinding = Option.some(signature.getLeft());
				final Identifier name = expectIdentifier(signature.getRight());
				return method(methodSourceExpr, name, List.nil(), selfBinding, Option.none(), body);
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
			 *
			 * ex:
			 *
			 * <pre>
			 * (b) c (d)
			 *
			 * (UNARY PAREN)    c
			 *            \    /
			 *            JUXTAPOSITION    d
			 *                       \    /
			 *                        CALL
			 * </pre>
			 */
			private DesugarResult<Slot> mixfixMethodPart(final BinaryOp juxtaposition) {
				return juxtaposition.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<Slot>>() {
					@Override
					public DesugarResult<Slot> fallback(SourceExpr other) {
						BadCoreExpr problem = new BadCoreExpr(juxtaposition.getSourceFileRanges(), "Invalid method signature");
						return withValue(new Slot(Identifier.UNDERSCORE, Option.none(), problem));
					}

					@Override
					public DesugarResult<Slot> binaryOp(BinaryOp op) {
						switch(op.getOperator()) {
						case NEWLINE:
						case JUXTAPOSITION:
						case CALL: {
							Identifier newNameSuffix = expectIdentifier(juxtaposition.getRight());
							List<SourceExpr> newArgumentListsSuffix = List.nil();
							return methodWithArgs(op, Option.some(newNameSuffix), newArgumentListsSuffix, CoreExprFactory.this);
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
			private DesugarResult<Slot> methodWithArgs(final BinaryOp call) {
				return methodWithArgs(call, Option.none(), List.nil(), CoreExprFactory.this);
			}

			/**
			 * Desugar the field definition as a method that takes parameters.
			 */
			private DesugarResult<Slot> methodWithArgs(final BinaryOp call, final Option<Identifier> nameSuffix, final List<SourceExpr> argumentListsSuffix, final CoreExprFactory ds) {
				final SourceExpr methodLeftExpr = call.getLeft();
				final SourceExpr argsExpr = call.getRight();
				return Objects.requireNonNull(methodLeftExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<Slot>>() {
					@Override
					public DesugarResult<Slot> binaryOp(BinaryOp methodDefBOp) {
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
					private DesugarResult<Slot> projection(BinaryOp methodDefBOp) {
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
					private DesugarResult<Slot> juxtaposition(final BinaryOp methodDefBOp) {
						// for a(b)c(d)
						// argsExpr = (d)
						// methodDefBOp.right = id(c)
						// methodDefBOp.left = call(id(a),(b))
						return methodDefBOp.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<Slot>>() {
							@Override
							public DesugarResult<Slot> fallback(SourceExpr other) {
								BadCoreExpr problem = new BadCoreExpr(methodDefBOp.getSourceFileRanges(), "Invalid method signature");
								return ds.withValue(new Slot(Identifier.UNDERSCORE, Option.none(), problem));
							}

							@Override
							public DesugarResult<Slot> binaryOp(BinaryOp op) {
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


					DesugarResult<Slot> apply(SourceExpr nameExpr, Option<SourceExpr> selfBinding, CoreExprFactory ds) {
						final Identifier key = expectIdentifier(nameExpr);
						return ds.method(
								methodSourceExpr,
								concatNameParts(key, nameSuffix),
								flattenArgumentLists(argumentListsSuffix.cons(argsExpr)),
								selfBinding,
								Option.none(),
								body);
					}
					@Override
					public DesugarResult<Slot> fallback(SourceExpr other) {
						return apply(methodLeftExpr, Option.none(), CoreExprFactory.this);
					}
				}));
			}

			/**
			 * Desugar the field as a method that doesn't take any parameters
			 */
			private DesugarResult<Slot> methodWithGuarantee(BinaryOp targetBOp) {
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
			public DesugarResult<Slot> fallback(SourceExpr other) {
				final Identifier key = expectIdentifier(signatureSourceExpr);
				return withValue(new Slot(key, Option.none(), body));
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
		final DesugarResult<List<Slot>> methodsDs = headings.zip(values).foldRight(new F2<P2<SourceExpr,SourceExpr>, DesugarResult<List<Slot>>, DesugarResult<List<Slot>>>() {
			@Override
			public DesugarResult<List<Slot>> f(
					P2<SourceExpr, SourceExpr> p,
					DesugarResult<List<Slot>> ds) {
				if(p == null || ds == null) throw new NullPointerException();
				final SourceExpr headingExpr = Objects.requireNonNull(p._1());
				final SourceExpr cellSourceExpr = Objects.requireNonNull(p._2());
				final List<Slot> tailMethods = ds.getValue();
				final DesugarResult<CoreExpr> cellDs = ds.expr(cellSourceExpr);
				return cellDs.addMethod(null, headingExpr, cellDs.getValue(), Identifier.TRUE, tailMethods, null);
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
		return Objects.requireNonNull(sourceExpr.acceptVisitor(new BaseSourceExprVisitor<SourceExpr>() {
			@Override
			public SourceExpr unaryOp(UnaryOp op) {
				switch(op.getOperator()) {
				case PARENS:
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
		return bodyDs.functionLiteral(sourceExpr, argsSourceExpr, body, Option.none());
	}

	protected DesugarResult<CoreExpr> functionLiteral(final SourceExpr sourceExpr,
			final SourceExpr argsSourceExpr, final CoreExpr body, final Option<SourceExpr> recursiveBinding) {
		return argsSourceExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
			@Override
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				return functionLiteral(sourceExpr, argsSourceExpr, recursiveBinding, body);
			}

			@Override
			public DesugarResult<CoreExpr> binaryOp(BinaryOp op) {
				if(op.getOperator() == Operator.CALL) {
					final Option<SourceExpr> newRecursiveBinding = Option.some(op.getLeft());
					final SourceExpr args = op.getRight();
					final DesugarResult<CoreExpr> result = functionLiteral(sourceExpr, args, body, newRecursiveBinding);
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
	protected DesugarResult<CoreExpr> functionLiteral(final SourceExpr methodSourceExpr, final SourceExpr args, final Option<SourceExpr> sourceObjectBinding, final CoreExpr body) {
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
				CoreExpr newBody = CoreExprFactory.this.applyGuarantee(body, op.getOperator(), newGuaranteeDs.getValue());
				return newGuaranteeDs.functionLiteral(methodSourceExpr, newArgs, sourceObjectBinding, newBody);
			}

			@Override
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				final List<SourceExpr> exprs = flattenCommas(stripParens(args));
				final DesugarResult<Slot> slotDs = method(
						methodSourceExpr,
						Identifier.UNDERSCORE,
						single(exprs),
						Option.none(),
						sourceObjectBinding,
						body);
				return slotDs.mapValue(slot -> slot.value);
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

	public DesugarResult<Slot> method(SourceExpr methodSourceExpr, Operator operator, SourceExpr other, Option<SourceExpr> selfBinding, CoreExpr body) {
		// TODO Allow unpacking in the self binding instead of requiring an identifier
		return functionLiteral(methodSourceExpr, other, Option.none(), body)
				.mapValue(func -> new Slot(operator.getMethodIdentifier(), selfBinding.map(CoreExprFactory::expectIdentifier), func));
	}

	protected DesugarResult<Slot> method(SourceExpr methodSourceExpr, final Identifier methodName, List<List<SourceExpr>> argumentListsSourceExprs, Option<SourceExpr> selfBinding, Option<SourceExpr> recursiveBinding, CoreExpr body) {
		return method(methodSourceExpr, methodName, argumentListsSourceExprs, selfBinding, recursiveBinding, Identifier.TRUE, body, Identifier.TRUE);
	}

	protected DesugarResult<Slot> method(final SourceExpr methodSourceExpr, final Identifier methodName, List<List<SourceExpr>> argumentListsSourceExprs, Option<SourceExpr> selfBinding, Option<SourceExpr> recursiveBinding, CoreExpr precondition, CoreExpr currBody, CoreExpr postcondition) {
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
				return argListDs.bindSelf(recursiveBinding, newBody).mapValue(p ->
					(CoreExpr)new FunctionLiteral(
							methodSourceExpr.getSourceFileRanges(),
							args,
							p._2(),
							p._1()));
		}, this.withValue(currBody));

		final Option<DesugarResult<Slot>> t1 = selfBinding.map(name ->
					funcDs.bindSelf(name, funcDs.getValue())
					.mapValue(p -> new Slot(methodName, p._1(), p._2())));
		return t1.orSome(P.lazy(() -> funcDs.mapValue(f -> new Slot(methodName, Option.none(), f))));
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
		final CoreExprFactory ds = CoreExprFactory.this;
		return Objects.requireNonNull(argExpr.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P3<CoreExpr, CoreExpr, List<Identifier>>>>() {

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
		Call check = new Call(constraint.getSourceFileRanges(), new Projection(parameterName, opMethodName(operator)), List.single(constraint));
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

		final List<SourceExpr> result = Objects.requireNonNull(arg.acceptVisitor(new BaseSourceExprVisitor<List<SourceExpr>>() {
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
		Set<SourceFileRange> operatorRanges = op.getOperatorRanges();
		final SourceExpr rightSourceExpr = op.getRight();
		switch(operator.operatorType) {
		case METHOD:
			return binaryOpToMethodCall(op, leftSourceExpr, operator, operatorRanges, rightSourceExpr, optional);
		case METHOD_SWITCHED:
			return binaryOpToMethodCall(op, rightSourceExpr, operator, operatorRanges, leftSourceExpr, optional);
		case FUNCTION:
			return binaryOpToFunctionCall(op, leftSourceExpr, rightSourceExpr, operator.getMethodIdentifier());
		case FUNCTION_SWITCHED:
			return binaryOpToFunctionCall(op, rightSourceExpr, leftSourceExpr, operator.getMethodIdentifier());
		default:
			throw new Error();
		}
	}

	/**
	 * For comparisons we allow chaining, like
	 *
	 * x == a == b means (x == a) && (a == b)
	 * a < b <= c means (a < b) && (b < c)
	 */
	protected DesugarResult<CoreExpr> comparisonOpToCall(final BinaryOp op) {
		return op.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
			@Override
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				return binaryOpToCall(op, false);
			}
			
			@Override
			public DesugarResult<CoreExpr> binaryOp(BinaryOp leftBOp) {
				switch(leftBOp.getOperator()) {
				case EQ:
				case NEQ:
				case LE:
				case LT:
				case GE:
				case GT:
					BinaryOp newRight = new BinaryOp(op.getSourceFileRanges(), op.getOperator(), op.getOperatorRanges(), leftBOp.getRight(), op.getRight());
					BinaryOp and = new BinaryOp(leftBOp.getSourceFileRanges(), Operator.LOGICAL_AND, SourceFileRange.EMPTY_SET, leftBOp, newRight);
					return binaryOpToCall(and, false);
					
				default: 
					return binaryOpToCall(op, false);
				}
			}
		});
	}

	protected DesugarResult<CoreExpr> binaryOpToMethodCall(SourceExpr op, final SourceExpr leftSourceExpr, final Operator operator, Set<SourceFileRange> operatorRanges, final SourceExpr rightSourceExpr, boolean optional) {
		final DesugarResult<CoreExpr> leftDs = expr(leftSourceExpr);
		final DesugarResult<CoreExpr> rightDs = leftDs.expr(rightSourceExpr);
		return rightDs.withDesugared(op, binaryOpToCall(op.getSourceFileRanges(), leftDs.getValue(), operator, operatorRanges, rightDs.getValue(), optional));
	}

	protected Call binaryOpToCall(Set<SourceFileRange> ranges, final CoreExpr leftCoreExpr, final Operator operator,	Set<SourceFileRange> operatorRanges, final CoreExpr rightCoreExpr, boolean optional) {
		final boolean rightAssoc = operator.isRightAssociative();
		final CoreExpr target = rightAssoc?rightCoreExpr:leftCoreExpr;
		final CoreExpr parameter = rightAssoc?leftCoreExpr:rightCoreExpr;
		final Identifier methodName = opMethodName(operatorRanges, operator);
		final CoreExpr callTarget = operator == Operator.CALL? target : new Projection(target, methodName);
		return new Call(ranges, callTarget, List.single(parameter));
	}


	protected DesugarResult<CoreExpr> nullaryFunctionLiteral(SourceExpr sourceExpr, SourceExpr body) {
		final DesugarResult<CoreExpr> bodyDs = expr(body);
		return bodyDs.functionLiteral(sourceExpr, EmptyExpr.SYNTHETIC_INSTANCE, bodyDs.getValue(), Option.none());
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
		case BASE_SLOT: return projection(op, true);
		case FUNCTION: return functionLiteral(op);
		case PROJECTION: return projection(op, false);
		case PROJECTION_OF_MEMBERS: return mapProjection(op, false, false);

		// Normal operators are translated into a method call
		case GT:
		case LT:
		case GE:
		case LE:
		case NEQ:
		case EQ:
			return comparisonOpToCall(op);
		case MEMBER_OF:
		case CMP:
		case POW:
		case MUL:
		case DIV:
		case ADD:
		case SUB:
		case INTERSECT:
		case XOR:
		case UNION:
		case LOGICAL_AND:
		case LOGICAL_OR:
		case FALLBACK:
		case EXTENSION:
		case FUNCTION_COMPOSITION_LEFT:
		case FUNCTION_COMPOSITION_RIGHT:
			return binaryOpToCall(op, false);

		case JUXTAPOSITION:
			return juxtaposition(op);

		case NEWLINE:
			return call(op);

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
		case LIST_LITERAL: return listLiteral(op, operandSourceExpr);
		case OBJECT_LITERAL: return objectLiteral(op, operandSourceExpr);

		case PARENS:
			return parens(op, operandSourceExpr);

		case NOT:
		case COMPLEMENT:
		case PLUS:
		case NEGATE:
		case ABSVALUE:
			return unaryOpToSlotReference(op);

		case INSPECT:
			return inspect(op);

		case PROJECTION_FUNCTION:
			return freeProjection(op);

		case EXTENSION_FUNCTION:
			return unaryOpToFunctionCall(op);

		case BASE_FUNCTION:
			return baseFunction(op);

		case INVALID:
			return withDesugared(op, new BadCoreExpr(op.getSourceFileRanges(), "Invalid unary operator"));
		default:
			return withDesugared(op, new BadCoreExpr(op.getSourceFileRanges(), "Operator not supported here: '"+op.getOperator()+"'"));

		}
	}

	@Override
	public DesugarResult<CoreExpr> stringLiteral(StringLiteral stringLiteral) {
		return withDesugared(stringLiteral, stringLiteral);
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

	public DesugarResult<CoreExpr> inspect(UnaryOp op) {
		final DesugarResult<CoreExpr> exprDs = expr(op.getOperand());
		return exprDs.withDesugared(op, new Inspect(op.getSourceFileRanges(), exprDs.getValue()));
	}

	private DesugarResult<CoreExpr> binaryOpToFunctionCall(BinaryOp op, SourceExpr first, SourceExpr second, Identifier functionIdentifier) {
	    final DesugarResult<CoreExpr> firstDs = expr(first);
		final DesugarResult<CoreExpr> secondDs = firstDs.expr(second);
		return secondDs.withDesugared(op, new Call(new Call(functionIdentifier, List.single(firstDs.getValue())), List.single(secondDs.getValue())));
    }

	public DesugarResult<CoreExpr> baseFunction(UnaryOp op) {
		Identifier functionName = expectIdentifier(op.getOperand());
	    return withDesugared(op, new BaseFunctionRef(op.getSourceFileRanges(), functionName));
    }

	public DesugarResult<CoreExpr> freeProjection(UnaryOp op) {
		final Identifier __tmp = Identifier.__TMP;
		P3<CoreExpr,CoreExpr,CoreExprFactory> ps = op.getOperand().acceptVisitor(new BaseSourceExprVisitor<P3<CoreExpr,CoreExpr,CoreExprFactory>>() {
			@Override
			public P3<CoreExpr,CoreExpr,CoreExprFactory> identifier(Identifier field) {
				DesugarResult<CoreExpr> preDs = projection(op, __tmp, field, false);
				CoreExpr precondition = preDs.getValue();
				DesugarResult<CoreExpr> bodyDs = preDs.projection(op, __tmp, field, false);
				CoreExpr body = bodyDs.getValue();
				return P.p(precondition, body, bodyDs);
			}

			@Override
			public P3<CoreExpr,CoreExpr,CoreExprFactory> binaryOp(BinaryOp op) {
				// Variant has arguments

//				DesugarResult<CoreExpr> preDs = desugar(new BinaryOp(
//						op.getOperator(),
//						op.getOperatorRanges(),
//						insertProjectionTargetOnLeft(__tmp, op.getLeft(), Operator.OPT_PROJECTION),
//						op.getRight()
//				));
//				CoreExpr precondition = preDs.getValue();
				DesugarResult<CoreExpr> bodyDs = desugar(new BinaryOp(
						op.getOperator(),
						op.getOperatorRanges(),
						insertProjectionTargetOnLeft(__tmp, op.getLeft(), Operator.PROJECTION),
						op.getRight()
				));
				CoreExpr body = bodyDs.getValue();
				return P.p(Identifier.TRUE, body, bodyDs);
			}

			@Override
			public P3<CoreExpr, CoreExpr, CoreExprFactory> unaryOp(UnaryOp op) {
				// Callback shorthand - .(x) == (f) -> f(x)
				if(op.getOperator() == Operator.PARENS) {
					CoreExpr precondition = Identifier.TRUE; // TODO
					DesugarResult<CoreExpr> bodyDs = call(op, __tmp, op.getOperand(), flattenCommas(op.getOperand()));
					return P.p(precondition, bodyDs.getValue(), bodyDs);
				}
			    return super.unaryOp(op);
			}
			@Override
			public P3<CoreExpr, CoreExpr, CoreExprFactory> fallback(SourceExpr other) {
				final BadCoreExpr expr = new BadCoreExpr(other.getSourceFileRanges(), "Expected identifier or method invokation: "+other);
				return P.p(expr, expr, withDesugared(other, expr));
			}
		});

		CoreExprFactory ds = ps._3();
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
		List<SourceExpr> namedValues = Objects.requireNonNull(op.getLeft().acceptVisitor(new BaseSourceExprVisitor<List<SourceExpr>>() {
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
		return bodyDs.localFunctionDef(letOp, name, args, bodyDs.getValue(), Option.none());
	}

	public DesugarResult<P2<Identifier, CoreExpr>> localFunctionDef(SourceExpr letOp, SourceExpr name, SourceExpr args, CoreExpr body, Option<Identifier> nameSuffix) {
		return name.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<Identifier, CoreExpr>>>() {
			public DesugarResult<P2<Identifier, CoreExpr>> function(Identifier name) {
				Identifier fullName = concatNameParts(name, nameSuffix);
				DesugarResult<CoreExpr> funcDs = func(Option.some(fullName));
				return funcDs.mapValue(f -> P.p(fullName, f));
			}

			protected DesugarResult<CoreExpr> func(final Option<SourceExpr> recursiveNameBinding) {
	            return functionLiteral(letOp, args, body, recursiveNameBinding);
            }

			@Override
			public DesugarResult<P2<Identifier, CoreExpr>> binaryOp(BinaryOp op) {
				if(op.getOperator() == Operator.JUXTAPOSITION) {
					Identifier rightName = expectIdentifier(op.getRight());
					return op.getLeft().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<Identifier, CoreExpr>>>() {
						@Override
						public DesugarResult<P2<Identifier, CoreExpr>> binaryOp(BinaryOp leftOp) {
							if(leftOp.getOperator() == Operator.CALL) {
								DesugarResult<CoreExpr> funcDs = func(Option.none());
								return localFunctionDef(letOp, leftOp.getLeft(), leftOp.getRight(), funcDs.getValue(), Option.some(rightName));
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
				return function(identifier);
			}

			@Override
            public CoreExprFactory.DesugarResult<fj.P2<Identifier,CoreExpr>> fallback(SourceExpr other) {
				return identifier(expectIdentifier(other));
			}
		});
	}


	private DesugarResult<CoreExpr> juxtaposition(final BinaryOp op) {
		return op.getRight().acceptVisitor(new BaseSourceExprVisitor<DesugarResult<CoreExpr>>() {
			private DesugarResult<CoreExpr> nonMixfixCall() {
				return CoreExprFactory.this.call(op);
			}

			@Override
			public DesugarResult<CoreExpr> fallback(SourceExpr other) {
				return nonMixfixCall();
			}

			@Override
            public CoreExprFactory.DesugarResult<CoreExpr> identifier(Identifier keyOnRight) {
				return op.getLeft().acceptVisitor(new SourceExprVisitor<DesugarResult<CoreExpr>>() {
					private DesugarResult<CoreExpr> makeMixfixCall(final BinaryOp opLeft) {
		                return call(opLeft).mapValue(call -> addNamePartToCall(call, keyOnRight));
		            }

					@Override
					public DesugarResult<CoreExpr> unaryOp(UnaryOp opLeft) {
						if(opLeft.getOperator() == Operator.PARENS) {
							return infixNameJuxtaposition(opLeft);
						}
					    return nonMixfixCall();
					}

					private DesugarResult<CoreExpr> infixNameJuxtaposition(SourceExpr arg) {
	                    return binaryOp(new BinaryOp(Operator.CALL, new Identifier(""), arg));
                    }

					@Override
					public DesugarResult<CoreExpr> binaryOp(final BinaryOp opLeft) {
						if(opLeft.getOperator() == Operator.CALL || opLeft.getOperator() == Operator.JUXTAPOSITION) {
							return makeMixfixCall(opLeft);
						} else {
							return nonMixfixCall();
						}
					}

					@Override
                    public DesugarResult<CoreExpr> stringLiteral(StringLiteral x) {
	                    return infixNameJuxtaposition(x);
                    }

					@Override
                    public DesugarResult<CoreExpr> numberLiteral(NumberLiteral x) {
	                    return infixNameJuxtaposition(x);
                    }

					@Override
                    public DesugarResult<CoreExpr> identifier(Identifier identifier) {
						return nonMixfixCall();
                    }

					@Override
                    public DesugarResult<CoreExpr> operator(OperatorRef operatorRef) {
						return nonMixfixCall();
                    }

					@Override
                    public DesugarResult<CoreExpr> badSourceExpr(BadSourceExpr badSourceExpr) {
						return nonMixfixCall();
                    }

					@Override
                    public DesugarResult<CoreExpr> emptyExpr(EmptyExpr emptyExpr) {
	                    return expr(op.getRight()); // Not sure if this is even possible, but just in case ...
                    }

					@Override
                    public DesugarResult<CoreExpr> badIdentifier(BadIdentifier badIdentifier) {
	                    return nonMixfixCall();
                    }

				});
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
                	public CoreExpr projection(Projection n) {
                		return n.projection.acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {
                			@Override
                			public CoreExpr identifier(Identifier slotName) {
                        	    return new Projection(n.getSourceFileRanges(), n.object, concatNameParts(slotName, keyOnRight));
                			}
                			
                			@Override
                			public CoreExpr fallback() {
                        	    return new BadCoreExpr(keyOnRight.getSourceFileRanges(), "Dangling function name part %s", keyOnRight.id);
                			}
						});
                	}

                	@Override
                	public CoreExpr fallback() {
                	    return new BadCoreExpr(keyOnRight.getSourceFileRanges(), "Dangling function name part %s", keyOnRight.id);
                	}
				});
            }

		});
	}


	/**
	 * Implement a unary op as a slot on the operand.
	 */
	private DesugarResult<CoreExpr> unaryOpToSlotReference(final UnaryOp op) {
	    final DesugarResult<CoreExpr> operandCoreExpr = expr(op.getOperand());
	    return operandCoreExpr.withDesugared(op, new Projection(op.getSourceFileRanges(), operandCoreExpr.getValue(), opMethodName(op.getOperator())));
    }

	/**
	 * Implement a unary op using a unary function
	 */
	DesugarResult<CoreExpr> unaryOpToFunctionCall(UnaryOp op) {
		Identifier functionName = opMethodName(op.getOperator());
	    final DesugarResult<CoreExpr> operandCoreExpr = expr(op.getOperand());
	    return operandCoreExpr.withDesugared(op, new Call(functionName, List.single(operandCoreExpr.getValue())));
	}

	/**
	 * Figure out the self-binding.  This is more complex because it allows the self-reference
	 * to be unpacked.
	 */
	protected DesugarResult<P2<Option<Identifier>, CoreExpr>> bindSelf(SourceExpr selfBinding, final CoreExpr body) {
		return selfBinding.acceptVisitor(new BaseSourceExprVisitor<DesugarResult<P2<Option<Identifier>, CoreExpr>>>() {
			@Override
			public DesugarResult<P2<Option<Identifier>, CoreExpr>> fallback(SourceExpr other) {
				return localVariableDef(selfBinding, Identifier.__TMP).mapValue(pairs -> P.p(Option.some(Identifier.__TMP), new Let(pairs, body)));
			}

			@Override
			public DesugarResult<P2<Option<Identifier>, CoreExpr>> identifier(Identifier identifier) {
			    return withValue(P.p(Option.some(identifier), body));
			}

			@Override
			public DesugarResult<P2<Option<Identifier>, CoreExpr>> emptyExpr(EmptyExpr emptyExpr) {
			    return withValue(P.p(Option.none(), body));
			}
		});
    }

	protected DesugarResult<P2<Option<Identifier>, CoreExpr>> bindSelf(Option<SourceExpr> selfBinding, final CoreExpr body) {
		if(selfBinding.isNone()) {
			return withValue(P.p(Option.none(), body));
		}
		return bindSelf(selfBinding.some(), body);
	}

	private SourceExpr insertProjectionTargetOnLeft(SourceExpr objectExpr,
            final SourceExpr right, Operator projectionType) {
	    SourceExpr rightSide = right.acceptVisitor(new BaseSourceExprVisitor<SourceExpr>() {
	    	@Override
	    	public SourceExpr fallback(SourceExpr other) {
	    	    return new BinaryOp(Operator.PROJECTION, objectExpr, other);
	    	}

	    	@Override
	    	public SourceExpr binaryOp(BinaryOp op) {
	    		if(op.getOperator().isInfix() &&
	    				op.getOperator().isLeftAssociative()) {
	    			return new BinaryOp(op.getSourceFileRanges(), op.getOperator(), op.getOperatorRanges(), op.getLeft().acceptVisitor(this), op.getRight());
	    		}
	    	    return super.binaryOp(op);
	    	}
	    });
	    return rightSide;
    }

    /**
     * List all files at the given path which might be a source file.
     */
    public static Stream<Path> listSourceFiles(Path path) {
        try {
            return Files
                .list(path)
                .filter(p -> p.getFileName().toString().charAt(0) != '.').sorted(new Comparator<Path>() {
                    @Override
                    public int compare(Path o1, Path o2) {
                        int cmp = -Boolean.compare(Files.isDirectory(o1), Files.isDirectory(o2));
                        if(cmp != 0)
                            return cmp;

                        for(Iterator<Path> i1 = o1.iterator(), i2 = o2.iterator(); i1.hasNext() || i2.hasNext();) {
                            if(!i1.hasNext())
                                return 1; // i2 has more elements
                            if(!i2.hasNext())
                                return -1; // i1 has more elements
                            final String s1 = i1.next().toString();
                            final String s2 = i2.next().toString();
                            cmp = s1.compareToIgnoreCase(s2);
                            if(cmp != 0)
                                return cmp;
                            cmp = s1.compareTo(s2);
                            if(cmp != 0)
                                return cmp;
                        }
                        return cmp;
                    }
                });
        } catch(IOException e) {
            return Stream.empty();
        }
    }

    public static TreeMap<String, Slot> extendSlots(TreeMap<String, Slot> a, TreeMap<String, Slot> b) {
        return b.values().foldLeft(CoreExprFactory::extendSlot, a);
    }

    public static TreeMap<String, Slot> extendSlot(TreeMap<String, Slot> slots, Slot binding) {
        return slots.set(
            binding.name.id,
            slots.get(binding.name.id)
                .map(prevSlot -> mergeSlots(prevSlot, binding))
                .orSome(binding));
    }

    /**
     * Desugar a file path into a slot. The self-name may be taken from the
     * filename, if present.
     */
    public Slot pathToSlot(Path p) {
        SourceExprFactory parser = new SourceExprFactory(p.getParent());
        String slotLhs = p.getFileName().toString();
        SourceExpr lhs;
        try {
            // If it's not a folder, strip the file extension
            if(!Files.isDirectory(p)) {
                slotLhs = slotLhs.replaceFirst("\\.[^.]*$", "");
            }
            lhs = parser.parse(slotLhs);
        } catch(IOException e) {
            throw new UncheckedIOException(e);
        }
        CoreExpr value = CoreExprFromFile.forPath(p);
        return addMethod(lhs, lhs, value, Identifier.TRUE, List.nil(), Operator.ASSIGNMENT).getValue().head();
    }

    /**
     * Create a new slot which contains the old slot value extended with the new
     * one, without messing up self-name bindings.
     */
    static Slot mergeSlots(Slot base, Slot extension) {
    
        // No source object binding, no problems
        if(base.sourceObjectBinding.isNone() && extension.sourceObjectBinding.isNone()) {
            return new Slot(base.name, new Extend(base.value, extension.value));
        }
    
        // If there's a source object binding, we have to make sure neither side
        // sees the other's self-binding
        // Ideally instead of __tmp we'd be using some kind of hygienic name.
        // Hm.
        return new Slot(base.name, Option.some(Identifier.__TMP),
            new Extend(
                slotToExpr(base, Identifier.__TMP),
                slotToExpr(extension, Identifier.__TMP)));
    }

    static CoreExpr slotToExpr(Slot slot, Identifier newName) {
        if(slot.sourceObjectBinding.isNone() || slot.sourceObjectBinding.some().id.equals(newName.id))
            return slot.value;
        return Let.single(slot.sourceObjectBinding.some(), newName, slot.value);
    }

    static TreeMap<String, Slot> mergeBinding(
        Slot binding, TreeMap<String, Slot> bindingMap) {
        return extendSlot(bindingMap, binding);
    }

    /**
     * Inspect a path to produce a CoreExpr. If the target path is a directory /
     * folder this desugars into an object literal representation of the file.
     * If it is a regular file, the file extension is used to determine the
     * format of the file.
     */
    public CoreExpr loadFromPath(Path path) {
        if(Files.isDirectory(path)) {
            return loadFromDirectory(path);
        }
        if(Files.isRegularFile(path)) {
            return loadFromFile(path);
        }
        return new BadCoreExpr(
            new SourceFileRange(path, FileRange.EMPTY),
            "Unsupported file type / extension at %s", path);
    }

    /**
     * Desugar a file into a directory.
     */
    public CoreExpr loadFromFile(Path path) {
        String filename = path.getFileName().toString();
        int dot = filename.lastIndexOf('.');
        String ext = (dot > 0 ? filename.substring(dot + 1) : "");
        if("txt".equals(ext)) {
            return loadTxtFile(path);
        } else if("banjo".equals(ext)) {
            return loadBanjoSourceFile(path);
        } else if("jar".equals(ext) || "zip".equals(ext)) {
            return loadZipFile(path);
        } else {
            // Don't know what to do with this type of file
            return new BadCoreExpr(
                new SourceFileRange(path, FileRange.EMPTY),
                "Unsupported file type / extension at %s", path);
        }
    }

    /**
     * Load a ZIP file as a folder - the root of the zip is treated as a folder
     * and converted into an object literal.
     */
    public CoreExpr loadZipFile(Path path) {
        try {
            FileSystem fs = FileSystems.newFileSystem(path, Thread.currentThread().getContextClassLoader());
            Path root = fs.getRootDirectories().iterator().next();
            return CoreExprFromFile.forPath(root);
        } catch(IOException e) {
            return new BadCoreExpr(
                new SourceFileRange(path, FileRange.EMPTY),
                "Failed to read project from %s: %s", path.toString(), e.toString());
        }
    }

    public CoreExpr loadTxtFile(Path path) {
        try {
            return new StringLiteral(new String(Files.readAllBytes(path), StandardCharsets.UTF_8));
        } catch(IOException e) {
            return new BadCoreExpr(
                new SourceFileRange(path, FileRange.EMPTY),
                "Failed to read text from %s: %s", path.toString(), e.toString());
        }
    }

    public CoreExpr loadBanjoSourceFile(Path path) {
        try {
            int size = (int) Files.size(path);
            if(size == 0)
                return new BadCoreExpr(new SourceFileRange(path, FileRange.EMPTY), "Empty file '" + path + "'");
            
            try(BufferedReader in = Files.newBufferedReader(path)) {
                final SourceExprFactory parser = new SourceExprFactory(path);
                SourceExpr parsed = parser.parse(new ParserReader(in, size));
                return desugar(parsed).getValue();
            } catch(IOException e) {
                return new BadCoreExpr(
                    new SourceFileRange(path, FileRange.EMPTY),
                    "Failed to read source code from %s: %s", path.toString(), e.toString());
            }
        } catch(IOException e) {
            return new BadCoreExpr(
                new SourceFileRange(path, FileRange.EMPTY),
                "Failed to read source code from %s: %s", path.toString(), e.toString());
        }
    }

    /**
     * Desugar a directory into an AST
     */
    public CoreExpr loadFromDirectory(Path path) {
        return loadFromDirectories(List.single(path));
    }

    /**
     * Desugar a directory search path into an AST
     */
    public ObjectLiteral loadFromDirectories(List<Path> paths) {
        Stream<Path> slotFiles = StreamSupport
            .stream(paths.spliterator(), false)
            .flatMap(CoreExprFactory::listSourceFiles);
        TreeMap<String, Slot> slots = slotFiles
            .map(this::pathToSlot)
            .reduce(
                TreeMap.empty(Ord.stringOrd),
                CoreExprFactory::extendSlot,
                CoreExprFactory::extendSlots);
        return new ObjectLiteral(slots.values());
    }

    /**
     * Given the a source path somewhere in a project, load the AST for the
     * whole project.
     * 
     * @param sourceFilePath
     *            Path to look for the project at
     * @return AST for the project
     */
    public ObjectLiteral loadProjectAstForSourcePath(Path sourceFilePath) {
        List<Path> rootPaths = projectSourcePathsForFile(sourceFilePath.toAbsolutePath());
        // Construct the project root object
        ObjectLiteral projectAst = loadFromDirectories(rootPaths);
        return projectAst;
    }

    /**
     * Try to find the first parent folder of the given path which contains a
     * file/folder named ".banjo". This is considered to be the project root.
     * <p>
     * If no ".banjo" exists in the given path or a parent, this returns
     * Option.none().
     */
    public static Option<Path> projectRootForPath(Path path) {
        Path tryPath = path;
        while(tryPath != null) {
            if(Files.exists(tryPath.resolve(".banjo")))
                return Option.some(tryPath);
            tryPath = tryPath.getParent();
        }
        return Option.none();
    }

    /**
     * Get the core library source search paths. These are read from a system
     * property. You can always set the system property using an environment
     * variable, e.g.
     * <p>
     * <code>
     * JAVA_TOOL_OPTIONS=-Dbanjo.path=banjo-core-lib-master/src:banjo-java-lib-master/src
     * </code>
     */
    public static List<Path> getGlobalSourcePaths() {
        String searchPath = System.getProperty(CoreExprFactory.LIB_PATH_SYS_PROPERTY, "");
        return List.list(searchPath.split(File.pathSeparator))
            .filter(s -> !s.isEmpty())
            .map(Paths::get);
    }

    /**
     * Find the full project source search path list for the given source file;
     * this includes the project the file is in plus the core library search
     * paths.
     */
    public static List<Path> projectSourcePathsForFile(Path sourceFile) {
        Option<Path> projectRoot = CoreExprFactory.projectRootForPath(sourceFile);
        List<Path> coreLibraryPaths = CoreExprFactory.getGlobalSourcePaths();
        return coreLibraryPaths.append(projectRoot.toList());
    }

}
