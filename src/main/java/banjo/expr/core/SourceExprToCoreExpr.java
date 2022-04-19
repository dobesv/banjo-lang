package banjo.expr.core;

import static fj.data.List.cons;
import static fj.data.List.single;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
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
import banjo.expr.util.PathUtils;
import banjo.expr.util.SourceFileRange;
import fj.F;
import fj.F2Functions;
import fj.F2;
import fj.Ord;
import fj.P;
import fj.P2;
import fj.P3;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

public class SourceExprToCoreExpr implements SourceExprVisitor<CoreExpr> {
	protected static final Set<SourceFileRange> NOT_FROM_SOURCE = SourceFileRange.EMPTY_SET;
	static final Set<String> EMPTY_STRING_SET = Set.empty(Ord.stringOrd);

	public static final TreeMap<SourceFileRange, Set<BadExpr>> EMPTY_ERROR_MAP = TreeMap.empty(SourceFileRange.ORD);
	static final fj.data.Set<SourceExpr> EMPTY_SOURCE_EXPR_SET = fj.data.Set.empty(SourceExpr.sourceExprOrd);
	public static final SourceExprToCoreExpr INSTANCE = new SourceExprToCoreExpr();
	public static final String LIB_PATH_SYS_PROPERTY = "banjo.path";

	/**
	 * Coming in we have a tree of basically just unary and binary operations and
	 * parens and atoms.
	 *
	 * Desugaring changes that into function calls, projections, object literals,
	 * etc..
	 */
	public CoreExpr desugar(SourceExpr rootNode) {
		return expr(rootNode);
	}

	public CoreExpr expr(SourceExpr sourceExpr) {
		return Objects.requireNonNull(sourceExpr.acceptVisitor(this));
	}

	/**
	 * Desugar a few exprs at once. The state is accumulated from one to the next so
	 * that the last element of the array has the accumulated cache from the
	 * previous desugarings.
	 */
	public CoreExpr[] exprs(SourceExpr parent, SourceExpr... sourceExprs) {
		final CoreExpr[] results = new CoreExpr[sourceExprs.length];
		for (int i = 0; i < sourceExprs.length; i++) {
			results[i] = expr(sourceExprs[i]);
		}
		return results;
	}

	/**
	 * Projection.
	 *
	 * @see ScopedExpr
	 * @param op           BinaryOp describing the projection (having PROJECTION as
	 *                     its operator)
	 * @param base         TODO
	 * @param sourceOffset Source offset to the start of op
	 */
	protected CoreExpr scoped(BinaryOp op) {
		return scoped(op, op.getLeft(), op.getRight());
	}

	/**
	 * Evaluate an expression using an object for binding resolution.
	 * 
	 * For example, {x=1}.x would have the value 1, because the .x means to retrieve
	 * the value x from the object {x=1}. The expression after the '.' can be any
	 * expression, and all the name lookups will use the object instead of the
	 * current local scope.
	 * 
	 * Scope resolution can have special arguments used for function calls and
	 * intermediate resolution of inherited properties. To set these, the syntax
	 * (args ==> object).expr set the scope args for the resolution of expr in
	 * object. Note that the scope args apply to all names resolved in that scope so
	 * when these are not set to the defaults it is best to resolve a single name.
	 *
	 * @param sourceExpr     Root source expression for the projection.
	 * @param objectExpr     Left-hand side of the projection; the "base" object
	 *                       we're projecting from
	 * @param projectionExpr Right-hand side of the projection
	 */
	protected CoreExpr scoped(final SourceExpr sourceExpr, final SourceExpr objectExpr,
			final SourceExpr projectionExpr) {
		P2<CoreExpr, CoreExpr> objectAndArgs = objectExpr
				.acceptVisitor(new BaseSourceExprVisitor<P2<CoreExpr, CoreExpr>>() {
					@Override
					public P2<CoreExpr, CoreExpr> fallback(SourceExpr other) {
						return P.p(expr(other), Nil.SYNTHETIC_INSTANCE);
					}

					@Override
					public P2<CoreExpr, CoreExpr> binaryOp(BinaryOp op) {
						if (op.getOperator() == Operator.BINDING_ARGS) {
							return P.p(expr(op.getRight()), expr(op.getLeft()));
						}
						return fallback(op);
					}
				});
		final CoreExpr body = expr(projectionExpr);
		CoreExpr object = objectAndArgs._1();
		CoreExpr args = objectAndArgs._2();
		return new ScopedExpr(sourceExpr.getRanges(), object, body, args);
	}

	/**
	 * <pre>
	 *  x*.foo == x.map((x) -> x.foo)
	 * </pre>
	 * 
	 * <pre>
	 *  x*.foo == x.map((x) -> x.foo)
	 * </pre>
	 * 
	 * @param callNext TODO
	 * @param optional TODO
	 */
	protected CoreExpr mapScoped(BinaryOp op, boolean callNext, boolean optional) {
		CoreExpr target = expr(op.getLeft());
		SourceExpr projection = op.getRight();
		return mapScoped(op, target, projection, callNext, optional);
	}

	protected CoreExpr mapScoped(SourceExpr op, CoreExpr target, SourceExpr bodyAst, boolean callNext,
			boolean optional) {
		final Identifier argName = Identifier.ARG_0;
		final CoreExpr projectionDs = scoped(op, argName, bodyAst);
		final CoreExpr projectFunc = BindingExpr.functionLiteral1(argName, projectionDs);
		final CoreExpr mapMethod = new ScopedExpr(target,
				new Identifier(Operator.FUNCTION_COMPOSITION_LEFT.methodName));
		final CoreExpr call = ScopedExpr.call1(op.getRanges(), mapMethod, projectFunc);
		return call;
	}

	/**
	 * 
	 * @param op
	 * @return
	 */
	protected CoreExpr pipeTo(final BinaryOp op) {
		CoreExpr leftDs = desugar(op.getLeft());
		CoreExpr rightDs = desugar(op.getRight());
		return ScopedExpr.call1(op.getRanges(), rightDs, leftDs);
	}

	protected CoreExpr pipeFrom(final BinaryOp op) {
		CoreExpr leftDs = desugar(op.getLeft());
		CoreExpr rightDs = desugar(op.getRight());
		return ScopedExpr.call1(op.getRanges(), leftDs, rightDs);
	}

	protected static Identifier concatNameParts(Identifier prefix, Identifier suffix) {
		return new Identifier(prefix.getRanges().union(suffix.getRanges()), prefix.indentColumn,
				(prefix.id + " _ " + suffix.id).trim());
	}

	protected static Identifier concatNameParts(Identifier prefix, Option<Identifier> nameSuffix) {
		return nameSuffix.map((suf) -> concatNameParts(prefix, suf)).orSome(prefix);
	}

	protected CoreExpr call(final BinaryOp op) {
		CoreExpr targetDs = expr(op.getLeft());
		final List<SourceExpr> argSourceExprs = op.getOperator() == Operator.CALL ? flattenCommas(op.getRight())
				: List.single(op.getRight());
		return call(op, targetDs, argSourceExprs);
	}

	protected CoreExpr call(final BinaryOp op, CoreExpr target, final List<SourceExpr> argAsts) {
		final SourceExpr callSourceExpr = op;
		final SourceExpr argsSourceExpr = op.getRight();
		return call(callSourceExpr, target, argsSourceExpr, argAsts);
	}

	private CoreExpr call(final SourceExpr callSourceExpr, CoreExpr target, final SourceExpr argsSourceExpr,
			final List<SourceExpr> argSourceExprs) {
		return elements(argsSourceExpr, argSourceExprs, null,
				argsDs -> ScopedExpr.call(callSourceExpr.getRanges(), target, argsDs));
	}

	private CoreExpr listLiteral(final SourceExpr sourceExpr, List<SourceExpr> list, Operator requireBullet) {
		return elements(sourceExpr, list, requireBullet, (ds) -> new ListLiteral(sourceExpr.getRanges(), ds));
	}

	/**
	 * Process a list of expressions, which may make use of the table feature.
	 *
	 * @param sourceOffset  Absolute source offset that the expressions'
	 *                      offsetFromParent is relative to
	 * @param list          List of expressions to process
	 * @param requireBullet If true, each expression should be a UnaryOp with this
	 *                      as the operator
	 * @param problems      List to add problems to if they are found
	 * @return A list of CoreExpr, each with offsetFromParent relative to the given
	 *         sourceOffset
	 */
	protected CoreExpr elements(SourceExpr ast, final List<SourceExpr> list, final Operator requireBullet,
			F<List<CoreExpr>, CoreExpr> cb) {

		// First scan for table rows. We accumulate rows until we find a table
		// header. If there are rows with no table header they'll fall out of
		// this and get handled next.
		return F2Functions
				.tuple((final List<SourceExpr> unprocessedElts, final List<CoreExpr> processedRows) -> cb
						.f(elements2(requireBullet, unprocessedElts, processedRows)))
				.f(splitElementsAtTableHeader(list));
	}

	private P2<List<SourceExpr>, List<CoreExpr>> splitElementsAtTableHeader(final List<SourceExpr> list) {
		return list.foldRight(
				(e, p) -> F2Functions.tuple((List<SourceExpr> unprocessedRows, List<CoreExpr> processedRows) -> e
						.acceptVisitor(new BaseSourceExprVisitor<P2<List<SourceExpr>, List<CoreExpr>>>() {
							@Override
							public P2<List<SourceExpr>, List<CoreExpr>> unaryOp(UnaryOp op) {
								if (op.getOperator() == Operator.TABLE_HEADER) {
									return table(unprocessedRows, processedRows, op);
								} else {
									return fallback(op);
								}
							}

							@Override
							public P2<List<SourceExpr>, List<CoreExpr>> fallback(SourceExpr other) {
								return P.p(unprocessedRows.cons(other), processedRows);
							}
						})).f(p),
				P.p(List.nil(), List.nil()));
	}

	private List<CoreExpr> elements2(final Operator requireBullet, final List<SourceExpr> unprocessedRows,
			final List<CoreExpr> processedRows) {
		return unprocessedRows.foldRight((e, ds) -> e.acceptVisitor(new BaseSourceExprVisitor<List<CoreExpr>>() {
			@Override
			public List<CoreExpr> unaryOp(UnaryOp op) {
				if (requireBullet != null) {
					if (op.getOperator() == requireBullet) {
						return visitElement(op.getOperand());
					} else {
						final List<CoreExpr> eltDs = visitElement(op);
						final BadCoreExpr err = new BadCoreExpr(op.getRanges(), "Expected " + requireBullet.getOp());
						return eltDs.cons(err);
					}
				}
				return visitElement(op);
			}

			public List<CoreExpr> visitElement(SourceExpr eltSourceExpr) {
				return ds.cons(expr(eltSourceExpr));
			}

			@Override
			public List<CoreExpr> fallback(SourceExpr other) {
				if (requireBullet != null) {
					return ds.cons(new BadCoreExpr(other.getRanges(), "Expected " + requireBullet.getOp()));
				}
				return visitElement(other);
			}
		}), processedRows);
	}

	private CoreExpr bindingList(SourceExpr ast, SourceExpr methodsAst) {
		return bindingList(ast, flattenCommas(methodsAst));
	}

	private CoreExpr bindingList(SourceExpr ast, final List<SourceExpr> bindingSourceExprs) {
		final List<CoreExpr> slots = bindingSourceExprs
				.map((SourceExpr bindingSourceExpr) -> bindingElement(bindingSourceExpr, List.nil()));

		return Extend.composeSlots(slots);
	}

	protected CoreExpr bindingElement(final SourceExpr fieldAst, final List<SourceExpr> headings) {
		return Objects.requireNonNull(fieldAst.acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
			@Override
			public CoreExpr binaryOp(BinaryOp op) {
				switch (op.getOperator()) {
				case ASSIGNMENT:
					return assignmentBindingElement(op, Operator.ASSIGNMENT);
				case ADD_METHOD:
					return assignmentBindingElement(op, Operator.ADD);
				case SUB_METHOD:
					return assignmentBindingElement(op, Operator.SUB);
				case MUL_METHOD:
					return assignmentBindingElement(op, Operator.MUL);
				case DIVID_METHOD:
					return assignmentBindingElement(op, Operator.DIV);
				case UNION_METHOD:
					return assignmentBindingElement(op, Operator.UNION);
				case AND_METHOD:
					return assignmentBindingElement(op, Operator.AND_METHOD);
				case OR_METHOD:
					return assignmentBindingElement(op, Operator.OR_METHOD);
				default:
					return fallback(op);
				}
			}

			@Override
			public CoreExpr unaryOp(UnaryOp op) {
				switch (op.getOperator()) {
				case TABLE_HEADER:
					return visitTableHeader(op);
				case TABLE_ROW:
					return visitTableRow(op);
				case PLUS:
					// When +x is found, that means compose x into this object
					return expr(op);
				default:
					return fallback(op);
				}
			}

			private CoreExpr visitTableHeader(UnaryOp headerOp) {
				return new BadCoreExpr(headerOp.getRanges(), "Tables not implemented");
			}

			private CoreExpr visitTableRow(UnaryOp headerOp) {
				return new BadCoreExpr(headerOp.getRanges(), "Tables not implemented");
			}

			@Override
			public CoreExpr identifier(Identifier id) {
				return assignmentBindingElement(id, id, Operator.ASSIGNMENT);
			}

			public CoreExpr assignmentBindingElement(BinaryOp fieldOp, Operator combiningOp) {
				final SourceExpr left = fieldOp.getLeft();
				final SourceExpr right = fieldOp.getRight();
				return assignmentBindingElement(left, right, combiningOp);
			}

			private CoreExpr assignmentBindingElement(SourceExpr lvalueExpr, SourceExpr valueAst,
					Operator combiningOp) {
				final CoreExpr eltDs = element(valueAst, headings);
				return bindingElement(fieldAst, lvalueExpr, eltDs, Identifier.TRUE, combiningOp);
			}

			@Override
			public CoreExpr fallback(SourceExpr other) {
				return new BadCoreExpr(other.getRanges(),
						"Expected assignment, method definition, or composition operator");
			}
		}));
	}

	protected CoreExpr element(SourceExpr sourceExpr, List<SourceExpr> headings) {
		if (headings.isEmpty()) {
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

	protected CoreExpr applyCombiningOpToBindingElement(CoreExpr binding, Operator combiningOp) {
		if (combiningOp == null || combiningOp == Operator.ASSIGNMENT)
			return binding;
		return binding.acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {
			@Override
			public CoreExpr binding(BindingExpr be) {
				// a += b ==> a = BOUND_BASE + b
				Identifier baseValueName = Identifier.BOUND_BASE;
				CoreExpr newArgs = Extend
						.composeSlots(List.arrayList(new BindingExpr(baseValueName, baseValueName), be.args));
				CoreExpr newValue = ScopedExpr.callBinaryOp(baseValueName, combiningOp, be.body);
				return new BindingExpr(be.name, newValue, newArgs);
			}

			@Override
			public CoreExpr fallback() {
				return new BadCoreExpr(binding.getRanges(), "Expected normal binding for use with operator %s",
						combiningOp.getOp());
			}
		});
	}

	public CoreExpr bindingElement(final SourceExpr bindingAst, final SourceExpr signatureSourceExpr,
			final CoreExpr body, final CoreExpr postcondition, Operator combiningOp) {
		CoreExpr slot = bindingElement(bindingAst, signatureSourceExpr, body, postcondition);
		return applyCombiningOpToBindingElement(slot, combiningOp);
	}

	/**
	 * Process a binding expression - this could be the left side of an assignment, or an
	 * expression using in the argument list of a function definition.
	 * 
	 * @param bindingAst
	 * @param signatureSourceExpr
	 * @param body
	 * @param postCondition
	 * @return
	 */
	protected CoreExpr bindingElement(final SourceExpr bindingAst, final SourceExpr signatureSourceExpr,
			final CoreExpr body, final CoreExpr postCondition) {
		final CoreExpr result = signatureSourceExpr.acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
			@Override
			public CoreExpr binaryOp(BinaryOp targetBOp) {
				switch (targetBOp.getOperator()) {
				case MEMBER_OF:
					return methodWithGuarantee(targetBOp);
				case CALL:
					return methodWithArgs(targetBOp);
				case JUXTAPOSITION:
					// a(b)c ... mixfix with no final argument list
					return mixfixMethodPart(targetBOp);
				case IN_SCOPE:
					// self.(x) = ... or self.x = ...
					return methodWithSelfNameAndNoArgs(targetBOp);
				default:
					return fallback(targetBOp);
				}
			}

			public CoreExpr unaryOpMethod(UnaryOp op) {
				// (-a) = xxx ==> a.\- = xxx
				// or (-{neg} = neg) ==> a.\- = a.neg

				SourceExpr selfBinding = op.getOperand();
				Identifier name = new Identifier(op.getOperator().getMethodName());
				CoreExpr args = bindingElement(signatureSourceExpr, selfBinding, Identifier.BOUND_SELF,
						Nil.SYNTHETIC_INSTANCE);
				return new BindingExpr(name, body, args);
			}

			@Override
			public CoreExpr unaryOp(final UnaryOp targetOp) {
				switch (targetOp.getOperator()) {
				case PARENS:
					return targetOp.getOperand().acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
						@Override
						public CoreExpr binaryOp(BinaryOp op) {
							Operator operator = op.getOperator();
							switch (operator.getOperatorType()) {
							case BUILTIN:
							case FUNCTION:
							case FUNCTION_SWITCHED:
								return fallback(op);
							default:
							}

							boolean selfOnRight = operator.isSelfOnRightMethodOperator();
							Option<SourceExpr> selfBinding = Option.some(selfOnRight ? op.getRight() : op.getLeft());
							SourceExpr other = selfOnRight ? op.getLeft() : op.getRight();
							Identifier name = opMethodName(operator);
							return method(bindingAst, operator, other, selfBinding, body).withName(name);
						}

						@Override
						public CoreExpr unaryOp(UnaryOp op) {
							switch (op.getOperator()) {
							case OBJECT_COMPOSITION:
							case LIST_LITERAL:
								return fallback(op);
							default:
							}
							if (op.getOperator().getMethodName() == null) {
								return fallback(op);
							}
							return unaryOpMethod(op);
						}

						@Override
						public CoreExpr fallback(SourceExpr other) {
							Identifier name = new Identifier("_");
							CoreExpr body = new BadCoreExpr(other.getRanges(), "Empty method signature");
							return new BindingExpr(name, body);
						}
					});
				case OBJECT_COMPOSITION:
					// In this case the signature is a pattern match, e.g.
					// {{a,b} = foo} becomes foo.{a,b}
					// {{a=a, b=c} = foo} becomes foo.{a=a, c=b}
					return new ScopedExpr(body, bindingList(bindingAst, targetOp.getOperand()));

				default:
					return unaryOpMethod(targetOp);
				}

			}

			private CoreExpr methodWithSelfNameAndNoArgs(final BinaryOp signature) {
				final Option<SourceExpr> selfBinding = Option.some(signature.getLeft());
				final Identifier name = expectIdentifier(signature.getRight());
				return method(bindingAst, name, List.nil(), selfBinding, body);
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
			private CoreExpr mixfixMethodPart(final BinaryOp juxtaposition) {
				return juxtaposition.getLeft().acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
					@Override
					public CoreExpr fallback(SourceExpr other) {
						BadCoreExpr problem = new BadCoreExpr(juxtaposition.getRanges(), "Invalid method signature");
						return new BindingExpr(Identifier.BOTTOM, problem);
					}

					@Override
					public CoreExpr binaryOp(BinaryOp op) {
						switch (op.getOperator()) {
						case NEWLINE:
						case JUXTAPOSITION:
						case CALL: {
							Identifier newNameSuffix = expectIdentifier(juxtaposition.getRight());
							List<SourceExpr> newArgumentListsSuffix = List.nil();
							return methodWithArgs(op, Option.some(newNameSuffix), newArgumentListsSuffix,
									SourceExprToCoreExpr.this);
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
			private CoreExpr methodWithArgs(final BinaryOp call) {
				return methodWithArgs(call, Option.none(), List.nil(), SourceExprToCoreExpr.this);
			}

			/**
			 * Desugar the field definition as a method that takes parameters.
			 */
			private CoreExpr methodWithArgs(final BinaryOp call, final Option<Identifier> nameSuffix,
					final List<SourceExpr> argumentListsSuffix, final SourceExprToCoreExpr ds) {
				final SourceExpr methodLeftExpr = call.getLeft();
				final SourceExpr argsExpr = call.getRight();
				return Objects.requireNonNull(methodLeftExpr.acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
					@Override
					public CoreExpr binaryOp(BinaryOp methodDefBOp) {
						switch (methodDefBOp.getOperator()) {
						case IN_SCOPE:
							return projection(methodDefBOp);
						case JUXTAPOSITION:
							return juxtaposition(methodDefBOp);
						default:
							return fallback(methodDefBOp);
						}
					}

					/**
					 * When we find a projection in the signature that means they are specifying a
					 * "self arg" - the name of the receiver of the method call.
					 */
					private CoreExpr projection(BinaryOp methodDefBOp) {
						final SourceExpr nameSourceExpr = methodDefBOp.getRight();
						final SourceExpr selfNameSourceExpr = methodDefBOp.getLeft();
						final SourceExpr selfBinding = selfNameSourceExpr;
						return apply(nameSourceExpr, Option.some(selfBinding), ds);
					}

					/**
					 * Desugar a method signature with multiple argument lists. The argument list we
					 * are getting should be pre-pended to the argument list we already have (if
					 * any?)
					 *
					 * The juxtaposition should split a signature like this:
					 *
					 * ex: a(b) c(d)
					 * 
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
					private CoreExpr juxtaposition(final BinaryOp methodDefBOp) {
						// for a(b)c(d)
						// argsExpr = (d)
						// methodDefBOp.right = id(c)
						// methodDefBOp.left = call(id(a),(b))
						return methodDefBOp.getLeft().acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
							@Override
							public CoreExpr fallback(SourceExpr other) {
								BadCoreExpr problem = new BadCoreExpr(methodDefBOp.getRanges(),
										"Invalid method signature");
								return (new BindingExpr(Identifier.BOTTOM, problem));
							}

							@Override
							public CoreExpr binaryOp(BinaryOp op) {
								switch (op.getOperator()) {
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

					CoreExpr apply(SourceExpr nameExpr, Option<SourceExpr> selfBinding, SourceExprToCoreExpr ds) {
						final Identifier key = expectIdentifier(nameExpr);
						return ds.method(bindingAst, concatNameParts(key, nameSuffix),
								flattenArgumentLists(argumentListsSuffix.cons(argsExpr)), selfBinding, body);
					}

					@Override
					public CoreExpr fallback(SourceExpr other) {
						return apply(methodLeftExpr, Option.none(), SourceExprToCoreExpr.this);
					}
				}));
			}

			/**
			 * Desugar the field as a method that doesn't take any parameters
			 */
			private CoreExpr methodWithGuarantee(BinaryOp targetBOp) {
				final SourceExpr newLvalueExpr = targetBOp.getLeft();
				final CoreExpr newGuaranteeDs = expr(targetBOp.getRight());
				final CoreExpr combinedGuarantee = composeGuarantees(postCondition, newGuaranteeDs);
				return bindingElement(bindingAst, newLvalueExpr, body, combinedGuarantee);
			}

			/**
			 * In this case the lvalue wasn't something we recognize normally so it should
			 * be an identifier, which is the name of the method.
			 */
			@Override
			public CoreExpr fallback(SourceExpr other) {
				final Identifier key = expectIdentifier(signatureSourceExpr);
				return new BindingExpr(key, body);
			}
		});
		return result;
	}

	/**
	 * Desugar something the MUST be an identifier. If it's not an identifier, a
	 * placeholder is returned - an instance of BadIdentifier - that can be used as
	 * an identifier to continue with desugaring but which will be reported as an
	 * error.
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
	 * The resulting expression with have offsetInParent relative to the
	 * newGuarantee.
	 *
	 * @param guarantee                Previous guarantee
	 * @param guaranteeSourceOffset    Absolute source offset of the guarantee
	 * @param newGuarantee             New guarantee to add onto it
	 * @param newGuaranteeSourceOffset Absolute source offset of the new guarantee
	 * @return A new CoreExpr representing the combined guarantee
	 */
	protected CoreExpr composeGuarantees(CoreExpr guarantee, CoreExpr newGuarantee) {
		if (guarantee.equals(Identifier.TRUE)) {
			return newGuarantee;
		} else if (newGuarantee.equals(Identifier.TRUE)) {
			return guarantee;
		} else {
			return ScopedExpr.callBinaryOp(guarantee, Operator.LOGICAL_AND, newGuarantee);
		}
	}

	/**
	 * Make a row from headings
	 *
	 * @param sourceExpr           Source expression for the row
	 * @param sourceOffset         Absolute source offset to the expression
	 * @param headings             Headings to use to create the record
	 * @param headingsSourceOffset Absolute source offset that the heading's
	 *                             offsetFromParent is relative to
	 * @return
	 */
	CoreExpr makeRow(SourceExpr sourceExpr, List<SourceExpr> headings) {
		return new BadCoreExpr(sourceExpr.getRanges(), "Not implemented");
//		List<BindingExpr> slots = headings.zip(flattenCommas(stripParens(sourceExpr)))
//				.foldRight(
//						(P2<SourceExpr, SourceExpr> p, List<BindingExpr> tailMethods) -> F2Functions
//								.tuple((final SourceExpr headingExpr, final SourceExpr cellSourceExpr) -> addMethod(
//										null, headingExpr, expr(cellSourceExpr), Identifier.TRUE, tailMethods, null))
//								.f(p),
//						List.<BindingExpr>nil());
//		Set<SourceFileRange> ranges = slots.foldLeft(
//				(Set<SourceFileRange> r, BindingExpr slot) -> r.union(slot.getRanges()), SourceFileRange.EMPTY_SET);
//		return bindingList(ranges, slots);
	}

	/**
	 * If the expression is wrapped in parentheses, remove them and return a new
	 * child expression with an offset reflecting the offset to the expression
	 * inside the parentheses.
	 */
	private SourceExpr stripParens(SourceExpr sourceExpr) {
		return Objects.requireNonNull(sourceExpr.acceptVisitor(new BaseSourceExprVisitor<SourceExpr>() {
			@Override
			public SourceExpr unaryOp(UnaryOp op) {
				switch (op.getOperator()) {
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
	 * @param sourceExpr   Function literal source expression, already identified as
	 *                     a BinaryOp with operator ->
	 * @param sourceOffset Absolute source offset of sourceExpr
	 */
	protected CoreExpr functionLiteral(BinaryOp sourceExpr) {
		final CoreExpr body = expr(sourceExpr.getRight());
		final SourceExpr argsSourceExpr = sourceExpr.getLeft();
		return functionLiteral(sourceExpr, argsSourceExpr, body, Option.none());
	}

	protected CoreExpr functionLiteral(final SourceExpr sourceExpr, final SourceExpr argsSourceExpr,
			final CoreExpr body, final Option<SourceExpr> recursiveBinding) {
		return argsSourceExpr.acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
			@Override
			public CoreExpr fallback(SourceExpr other) {
				return functionLiteral(sourceExpr, argsSourceExpr, recursiveBinding, body);
			}

			@Override
			public CoreExpr binaryOp(BinaryOp op) {
				// Check if the function has a name for itself
				if (op.getOperator() == Operator.CALL) {
					final Option<SourceExpr> newRecursiveBinding = Option.some(op.getLeft());
					final SourceExpr args = op.getRight();
					final CoreExpr result = functionLiteral(sourceExpr, args, body, newRecursiveBinding);
					return result;
				} else {
					return fallback(op);
				}
			}
		});
	}

	/**
	 * Create a function literal from an args definition (still in source form) and
	 * a body (already desugared).
	 * 
	 * @param args                  Argument source expression
	 * @param body                  Function body as a core expression
	 * @param sourceOffset          Absolute source offset of sourceExpr
	 * @param argsSourceOffset      Absolute offset in characters to the args
	 * @param bodySourceOffset      Absolute source offset of the function body
	 *                              expression
	 * @param guaranteeSourceOffset Absolute source offset of the guarantee; use the
	 *                              same offset as sourceOffset if none specified
	 */
	protected BindingExpr functionLiteral(final SourceExpr methodSourceExpr, final SourceExpr args,
			final Option<SourceExpr> calleeBinding, final CoreExpr body) {
		return args.acceptVisitor(new BaseSourceExprVisitor<BindingExpr>() {
			@Override
			public BindingExpr binaryOp(BinaryOp op) {
				switch (op.getOperator()) {
				case MEMBER_OF:
					// (x,y):Guarantee = ...
					return applyGuarantee(op);
				default:
					return fallback(op);
				}
			}

			private BindingExpr applyGuarantee(BinaryOp op) {
				final SourceExpr newArgs = op.getLeft();
				final SourceExpr newGuaranteeSourceExpr = op.getRight();
				final CoreExpr newGuaranteeDs = expr(newGuaranteeSourceExpr);
				CoreExpr newBody = SourceExprToCoreExpr.this.applyGuarantee(body, op.getOperator(), newGuaranteeDs);
				return functionLiteral(methodSourceExpr, newArgs, calleeBinding, newBody);
			}

			@Override
			public BindingExpr fallback(SourceExpr other) {
				final List<SourceExpr> exprs = flattenCommas(stripParens(args));
				final BindingExpr slot = method(methodSourceExpr, Identifier.UNDERSCORE, single(exprs), calleeBinding,
						body);
				// Although it's not reflected in the types, method must only return a binding with
				// a binding in its body
				BindingExpr funcBinding = (BindingExpr) slot.body;
				// For a method, BOUND_SELF and BOUND_BASE are set to the containing object, but for
				// a function literal they refer to the actual function itself.  So we need to merge
				// any self args from the slot definition into the slot body (which has the function
				// definition)
				if(Nil.isNil(slot.args) || Nil.isNil(funcBinding)) {
					// Short-circuit for simple cases of no args or no body
					return funcBinding;
				}
				return new BindingExpr(funcBinding.name, funcBinding.body, Extend.extension(slot.args, funcBinding.args));
			}
		});

	}

	/**
	 * Take a list of SourceExpr that might be a comma-separated list of arguments
	 * and make that into a list of arguments lists with the arguments split by
	 * commas.
	 */
	List<List<SourceExpr>> flattenArgumentLists(List<SourceExpr> a) {
		return a.map(x -> flattenCommas(x));
	}

	public BindingExpr method(SourceExpr methodSourceExpr, Operator operator, SourceExpr other,
			Option<SourceExpr> selfBinding, CoreExpr body) {
		CoreExpr args = selfBinding.map(sb -> bindingExprArgsFromSelfRefExpr(sb)).orSome(Nil.SYNTHETIC_INSTANCE);
		return new BindingExpr(new Identifier(operator.getMethodName()),
				functionLiteral(methodSourceExpr, other, Option.none(), body), args);
	}

	private CoreExpr bindingExprArgsFromSelfRefExpr(SourceExpr selfRefExpr) {
		// When a binding is written like `selfName.slotName = body` we allow
		// selfName to be a binding pattern, e.g. `{x}.slotName = x + 1` fetches
		// x from the same object and uses it in the body.
		// Also for special cases you can use parens to explicitly bind BOUND_SELF,
		// BOUND_BASE, _0, and so on. Example: `({x = BOUND_BASE, y = BOUND_SELF}).x = x
		// - 1`
		// The binding args can also be used to insert some constants into scope, as
		// with
		// a closure after / during (partial) evaluation.
		return selfRefExpr.acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
			/**
			 * Just unwrap parens here, bindingElement will do the rest of the work
			 */
			@Override
			public CoreExpr unaryOp(UnaryOp op) {
				if (op.getOperator() == Operator.PARENS) {
					// ({...}).x = ... => ({...}).x = ...
					return expr(op.getOperand());
				}
				return fallback(op);
			}

			@Override
			public CoreExpr fallback(SourceExpr other) {
				// x.y = ... => ({x = BOUND_SELF}).y = ...
				CoreExpr be = bindingElement(selfRefExpr, selfRefExpr, Identifier.BOUND_SELF, Nil.SYNTHETIC_INSTANCE);
				return be;
			}
		});
	}

	protected BindingExpr method(SourceExpr methodSourceExpr, final Identifier methodName,
			List<List<SourceExpr>> argumentListsSourceExprs, Option<SourceExpr> selfBinding, CoreExpr body) {
		return method(methodSourceExpr, methodName, argumentListsSourceExprs, selfBinding, Identifier.TRUE, body,
				Identifier.TRUE);
	}

	protected BindingExpr applyPrecondition(BindingExpr b, CoreExpr precondition) {
		if (Nil.isNil(precondition) || Identifier.TRUE.eql(precondition)) {
			// No precondition, really
			return b;
		}

		CoreExpr newArgs = Extend.extension(b.args, new BindingExpr(Identifier.CONTRACT_PRECONDITION, precondition));
		CoreExpr newBody = ScopedExpr.call(KernelGlobalObject.APPLY_PRECONDITION,
				List.arrayList(Identifier.CONTRACT_PRECONDITION, b.body));
		return new BindingExpr(b.name, newBody, newArgs);
	}

	protected BindingExpr applyPostcondition(BindingExpr b, CoreExpr postcondition) {
		if (Nil.isNil(postcondition) || Identifier.TRUE.eql(postcondition)) {
			// No precondition, really
			return b;
		}

		CoreExpr newArgs = Extend.extension(b.args, new BindingExpr(Identifier.CONTRACT_POSTCONDITION, postcondition));
		CoreExpr newBody = ScopedExpr.call(KernelGlobalObject.APPLY_POSTCONDITION,
				List.arrayList(Identifier.CONTRACT_POSTCONDITION, b.body));
		return new BindingExpr(b.name, newBody, newArgs);
	}

	protected BindingExpr method(final SourceExpr methodSourceExpr, final Identifier methodName,
			final List<List<SourceExpr>> argumentListsSourceExprs, final Option<SourceExpr> selfBinding,
			final CoreExpr precondition, final CoreExpr currBody, final CoreExpr postcondition) {
		// FIXME preconditions/postcondition should go in the inner most body
		// FIXME selfBinding/slotArgs should go in the outer function
		final CoreExpr allPreconditions = argumentListsSourceExprs.map(this::calculateMethodPreconditions)
				.foldRight(this::composePreconditions, Nil.SYNTHETIC_INSTANCE);
		final BindingExpr baseInnerBindingExpr = applyPostcondition(
				applyPrecondition(new BindingExpr(Identifier.BETA_REDUCTION, currBody, Nil.SYNTHETIC_INSTANCE),
						allPreconditions),
				postcondition);
		final BindingExpr outer = argumentListsSourceExprs
				.foldRight((F2<List<SourceExpr>, BindingExpr, BindingExpr>) (argsSourceExprs, inner) -> {
					CoreExpr argBindings = calculateMethodArgBindings(argsSourceExprs);
					CoreExpr updatedInner = new BindingExpr(inner.name, inner.body,
							Extend.extension(argBindings, inner.args));
					return new BindingExpr(Identifier.BETA_REDUCTION, updatedInner, Nil.SYNTHETIC_INSTANCE);
				}, baseInnerBindingExpr);
		CoreExpr slotArgs = selfBinding.map(sb -> bindingExprArgsFromSelfRefExpr(sb)).orSome(Nil.SYNTHETIC_INSTANCE);
		return new BindingExpr(methodName, outer.body, Extend.extension(slotArgs, outer.args));
	}

	private CoreExpr calculateMethodArgBindings(List<SourceExpr> argsListSourceExprs) {
		return Extend.composeSlots(argsListSourceExprs.zipIndex().map(F2Functions.tuple(
				(SourceExpr argExpr, Integer index) -> methodFormalArgumentBindings(argExpr, Identifier.arg(index)))));
	}

	public CoreExpr methodFormalArgumentBindings(final SourceExpr argExpr, Identifier argId) {
		return argExpr.acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {

			@Override
			public CoreExpr binaryOp(final BinaryOp argOp) {
				switch (argOp.getOperator()) {
				case MEMBER_OF:
				case EQ:
				case GE:
				case GT:
				case LE:
				case LT:
				case NEQ:
					final Identifier paramDecl = expectIdentifier(argOp.getLeft());
					return new BindingExpr(paramDecl, argId);

				default:
					return fallback(argOp);
				}
			}

			@Override
			public CoreExpr identifier(Identifier key) {
				return new BindingExpr(key, argId);
			}

			@Override
			public CoreExpr fallback(SourceExpr other) {
				return assignmentBinding(other, argId);
			}
		});
	}

	private CoreExpr calculateMethodPreconditions(List<SourceExpr> argsListSourceExprs) {
		return argsListSourceExprs
				.zipIndex().map(F2Functions.tuple((SourceExpr argExpr,
						Integer index) -> methodFormalArgumentPrecondition(argExpr, Identifier.arg(index))))
				.foldRight(this::composePreconditions, Nil.SYNTHETIC_INSTANCE);

	}

	public CoreExpr methodFormalArgumentPrecondition(final SourceExpr argExpr, Identifier argId) {
		return argExpr.acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {

			@Override
			public CoreExpr binaryOp(final BinaryOp argOp) {
				switch (argOp.getOperator()) {
				case MEMBER_OF:
				case EQ:
				case GE:
				case GT:
				case LE:
				case LT:
				case NEQ:
					// We have a precondition to check!
					return ScopedExpr.callBinaryOp(argId, argOp.getOperator(), expr(argOp.getRight()));

				default:
					return Nil.SYNTHETIC_INSTANCE;
				}
			}

			@Override
			public CoreExpr identifier(Identifier key) {
				return Nil.SYNTHETIC_INSTANCE;
			}

			@Override
			public CoreExpr fallback(SourceExpr other) {
				return Nil.SYNTHETIC_INSTANCE;
			}
		});
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
	protected CoreExpr insertContract(CoreExpr currentPrecondition, Identifier parameterName, Operator operator,
			CoreExpr constraint) {
		CoreExpr check = ScopedExpr.callBinaryOp(parameterName, operator, constraint);
		return composePreconditions(currentPrecondition, check);
	}

	private CoreExpr composePreconditions(CoreExpr a, CoreExpr b) {
		if (Nil.isNil(a) || a.equals(Identifier.TRUE)) {
			return b;
		} else if (Nil.isNil(b) || b.equals(Identifier.TRUE)) {
			return a;
		} else {
			return ScopedExpr.callBinaryOp(a, Operator.LOGICAL_AND, b);
		}
	}

	protected CoreExpr applyGuarantee(CoreExpr currentPostcondition, Operator operator, CoreExpr constraint) {
		throw new Error("Not implemented ...");
	}

	private List<SourceExpr> flattenList(final SourceExpr arg, final Operator sep) {

		final List<SourceExpr> result = Objects
				.requireNonNull(arg.acceptVisitor(new BaseSourceExprVisitor<List<SourceExpr>>() {
					@Override
					public List<SourceExpr> emptyExpr(EmptyExpr emptyExpr) {
						// Don't add anything for an empty expression
						return List.nil();
					}

					@Override
					public List<SourceExpr> binaryOp(BinaryOp op) {
						if (op.getOperator() == sep || op.getOperator() == Operator.NEWLINE) {

							final List<SourceExpr> left = flattenList(op.getLeft(), sep);

							final List<SourceExpr> right = flattenList(op.getRight(), sep);
							return left.isEmpty() ? right
									: right.isEmpty() ? left
											: left.tail().isEmpty() ? cons(left.head(), right) : left.append(right);
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

	protected CoreExpr binaryOpToCall(final BinaryOp op, boolean optional) {
		final SourceExpr leftSourceExpr = op.getLeft();
		final Operator operator = op.getOperator();
		Set<SourceFileRange> operatorRanges = op.getOperatorRanges();
		final SourceExpr rightSourceExpr = op.getRight();
		switch (operator.operatorType) {
		case METHOD:
			return binaryOpToMethodCall(op, leftSourceExpr, operator, operatorRanges, rightSourceExpr, optional);
		case METHOD_SWITCHED:
			return binaryOpToMethodCall(op, rightSourceExpr, operator, operatorRanges, leftSourceExpr, optional);
		case FUNCTION:
			return binaryOpToFunctionCall(op, leftSourceExpr, rightSourceExpr, operator);
		case FUNCTION_SWITCHED:
			return binaryOpToFunctionCall(op, rightSourceExpr, leftSourceExpr, operator);
		default:
			throw new Error();
		}
	}

	/**
	 * For comparisons we allow chaining, like
	 *
	 * x == a == b means (x == a) && (a == b) a < b <= c means (a < b) && (b < c)
	 */
	protected CoreExpr comparisonOpToCall(final BinaryOp op) {
		return op.getLeft().acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
			@Override
			public CoreExpr fallback(SourceExpr other) {
				switch (op.getOperator()) {
				case LE:
					return orEqual(Operator.LT);
				case GE:
					return orEqual(Operator.GT);
				case NEQ:
					return negated(Operator.EQ);
				default:
					return binaryOpToCall(op, false);
				}
			}

			public CoreExpr orEqual(Operator bareOp) {
				BinaryOp bareCmp = new BinaryOp(op.getRanges(), bareOp, op.getOperatorRanges(), op.getLeft(),
						op.getRight());
				BinaryOp eq = new BinaryOp(op.getRanges(), Operator.EQ, op.getOperatorRanges(), op.getLeft(),
						op.getRight());
				BinaryOp or = new BinaryOp(op.getRanges(), Operator.LOGICAL_OR, op.getOperatorRanges(), bareCmp, eq);
				return binaryOpToCall(or, false);
			}

			public CoreExpr negated(Operator bareOp) {
				BinaryOp bareCmp = new BinaryOp(op.getRanges(), bareOp, op.getOperatorRanges(), op.getLeft(),
						op.getRight());
				UnaryOp neg = new UnaryOp(op.getRanges(), Operator.NOT, op.getOperatorRanges(), bareCmp);
				return unaryOpToSlotReference(neg);
			}

			@Override
			public CoreExpr binaryOp(BinaryOp leftBOp) {
				switch (leftBOp.getOperator()) {
				case EQ:
				case NEQ:
				case LE:
				case LT:
				case GE:
				case GT:
					BinaryOp newRight = new BinaryOp(op.getRanges(), op.getOperator(), op.getOperatorRanges(),
							leftBOp.getRight(), op.getRight());
					BinaryOp and = new BinaryOp(leftBOp.getRanges(), Operator.LOGICAL_AND, SourceFileRange.EMPTY_SET,
							leftBOp, newRight);
					return binaryOpToCall(and, false);

				default:
					return fallback(op);
				}
			}
		});
	}

	protected CoreExpr binaryOpToMethodCall(SourceExpr op, final SourceExpr leftSourceExpr, final Operator operator,
			Set<SourceFileRange> operatorRanges, final SourceExpr rightSourceExpr, boolean optional) {
		final CoreExpr leftDs = expr(leftSourceExpr);
		final CoreExpr rightDs = expr(rightSourceExpr);
		return binaryOpToCall(op.getRanges(), leftDs, operator, operatorRanges, rightDs, optional);
	}

	protected CoreExpr binaryOpToCall(Set<SourceFileRange> ranges, final CoreExpr leftCoreExpr, final Operator operator,
			Set<SourceFileRange> operatorRanges, final CoreExpr rightCoreExpr, boolean optional) {
		final boolean rightAssoc = operator.isRightAssociative();
		final CoreExpr target = rightAssoc ? rightCoreExpr : leftCoreExpr;
		final CoreExpr parameter = rightAssoc ? leftCoreExpr : rightCoreExpr;
		final Identifier methodName = opMethodName(operatorRanges, operator);
		final CoreExpr callTarget = operator == Operator.CALL ? target : new ScopedExpr(target, methodName);
		return ScopedExpr.call1(ranges, callTarget, parameter);
	}

	protected CoreExpr nullaryFunctionLiteral(SourceExpr sourceExpr, SourceExpr body) {
		return functionLiteral(sourceExpr, EmptyExpr.SYNTHETIC_INSTANCE, expr(body), Option.none());
	}

	protected CoreExpr singletonListLiteral(UnaryOp op) {
		return new ListLiteral(op.getRanges(), single(expr(op.getOperand())));
	}

	protected CoreExpr listLiteral(SourceExpr sourceExpr, final SourceExpr elementsExpr) {
		final List<SourceExpr> exprs = flattenCommas(elementsExpr);
		return listLiteral(elementsExpr, exprs, null);
	}

	private CoreExpr parens(SourceExpr sourceExpr, SourceExpr body) {
		return expr(body);
	}

	@Override
	public CoreExpr binaryOp(final BinaryOp op) {
		// Comma outside of a parentheses should be a list or map without the
		// braces/brackets
		switch (op.getOperator()) {
		// case SEMICOLON:
		// case NEWLINE: {
		// return exprPair(op);
		// }
		case CALL:
			return call(op);
		case PASS_TO:
			return pipeTo(op);
		case PASS_TO_LEFT:
			return pipeFrom(op);

		// '.' and variants with NO parameters. When there's a call, these are
		// checked for specially inside of call().
		case FUNCTION_ARROW:
			return functionLiteral(op);
		case IN_SCOPE:
			return scoped(op);
		case PROJECTION_OF_MEMBERS:
			return mapScoped(op, false, false);

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
		case FUNCTION_COMPOSITION_LEFT:
		case FUNCTION_COMPOSITION_RIGHT:
			return binaryOpToCall(op, false);

		case JUXTAPOSITION:
			return juxtaposition(op);

		case NEWLINE:
			return call(op);

		case WITH_SCOPE:
			return let(op);
		default:
			return new BadCoreExpr(op.getRanges(), "Operator not supported here: '" + op.getOperator() + "'");
		}
	}

	@Override
	public CoreExpr unaryOp(final UnaryOp op) {
		final SourceExpr operandSourceExpr = op.getOperand();
		switch (op.getOperator()) {
		case LIST_ELEMENT:
			return singletonListLiteral(op);
		case NULLARY_FUNCTION_LITERAL:
			return nullaryFunctionLiteral(op, op.getOperand());
		case LIST_LITERAL:
			return listLiteral(op, operandSourceExpr);
		case OBJECT_COMPOSITION:
			return bindingList(op, operandSourceExpr);

		case PARENS:
			return parens(op, operandSourceExpr);

		case NOT:
		case COMPLEMENT:
		case PLUS:
		case NEGATE:
		case ABSVALUE:
			return unaryOpToSlotReference(op);

		case PROJECTION_FUNCTION:
			return freeProjection(op);

		case INSPECT:
		case EXTENSION_FUNCTION:
		case PASS_TO_LEFT_FUNCTION:
			return unaryOpToFunctionCall(op);

		case INVALID:
			return new BadCoreExpr(op.getRanges(), "Invalid unary operator");
		default:
			return new BadCoreExpr(op.getRanges(), "Operator not supported here: '" + op.getOperator() + "'");

		}
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
	public CoreExpr identifier(Identifier simpleName) {
		try {
			KernelGlobalObject kernelObject = KernelGlobalObject.valueOf(simpleName.id);
			return kernelObject;
		} catch (IllegalArgumentException ie) {
			return simpleName;
		}
	}

	@Override
	public CoreExpr operator(OperatorRef operatorRef) {
		return operatorRef;
	}

	// @Override
	// public CoreExpr visitWhitespace(Whitespace ws) {
	// return null;
	// }
	//
	// @Override
	// public CoreExpr visitComment(Comment c) {
	// return null;
	// }
	//
	// @Override
	// public CoreExpr visitEof() {
	// return null;
	// }

	@Override
	public CoreExpr badSourceExpr(BadSourceExpr badSourceExpr) {
		return new BadCoreExpr(badSourceExpr.getRanges(), badSourceExpr.getMessage());
	}

	@Override
	public CoreExpr emptyExpr(EmptyExpr emptyExpr) {
		return new Nil(emptyExpr.getRanges());
	}

	@Override
	public CoreExpr badIdentifier(BadIdentifier badIdentifier) {
		return new BadCoreExpr(badIdentifier.getRanges(), badIdentifier.getMessage());
	}

	private CoreExpr binaryOpToFunctionCall(BinaryOp op, SourceExpr first, SourceExpr second, Operator operator) {
		return ScopedExpr.call1(op.getRanges(),
				ScopedExpr.call1(first.getRanges(), operator.methodIdentifier, expr(first)), expr(second));
	}

	/**
	 * .<projection> --> (__target ↦ __target.<projection>)
	 * 
	 * Note that we don't have to worry about identifier hygiene here, the
	 * projection is evaluated in the scope of the target object and cannot see our
	 * temporary variable.
	 */
	public CoreExpr freeProjection(UnaryOp op) {
		final Identifier __tmp = new Identifier(op.getRanges(), 0, "__target");
		return BindingExpr.functionLiteral1(__tmp, scoped(op, __tmp, op.getOperand()));
	}

	public CoreExpr extend(BinaryOp op) {
		return new Extend(op.getRanges(), expr(op.getLeft()), expr(op.getRight()));
	}

	public CoreExpr let(final BinaryOp op) {
		CoreExpr object = expr(op.getLeft());
		CoreExpr body = expr(op.getRight());
		return ScopedExpr.let(op.getRanges(), object, body);
	}

	public CoreExpr assignment(SourceExpr letOp, SourceExpr left, SourceExpr right) {
		return left.acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
			@Override
			public CoreExpr binaryOp(BinaryOp op) {
				switch (op.getOperator()) {
				case JUXTAPOSITION:
				case CALL: {
					return functionShorthand(letOp, op.getLeft(), op.getRight(), right);
				}
				default:
					return fallback(op);
				}
			}

			@Override
			public CoreExpr fallback(SourceExpr argPattern) {
				return assignmentBinding(left, right);
			}

		});
	}

	/**
	 * Take an assignment and return an binding object. This takes the left and
	 * right side of the assignment separately. If the left side is a pattern, it
	 * unpacks the pattern. If pattern matching results in multiple bindings, this
	 * may give an expression for an object with the resulting bindings in it.
	 * 
	 * @param lhs   Left side of the assignment expr - identifier or pattern
	 * @param value Right side of the assignment expr - value to assign/unpack
	 * @return BindingExpr or other CoreExpr for multiple bindings
	 */
	public CoreExpr assignmentBinding(final SourceExpr lhs, final SourceExpr value) {
		return lhs.acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
			@Override
			public CoreExpr unaryOp(UnaryOp op) {
				switch (op.getOperator()) {
				case OBJECT_COMPOSITION:
					return unpackObject(op);
				default:
					return fallback(op);
				}
			}

			private CoreExpr unpackObject(UnaryOp op) {
				// {a, b} = x becomes x.{a, b}
				// {a = aa, b = bb} = x becomes +(x.{aa = a, bb = b})
				CoreExpr valueDs = expr(value);
				CoreExpr projectionBody = Extend.composeSlots(flattenCommas(op.getOperand()).map(this::reverseBinding));
				return new ScopedExpr(valueDs, projectionBody);
			}

			private CoreExpr reverseBinding(SourceExpr binding) {
				return binding.acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
					@Override
					public CoreExpr identifier(Identifier identifier) {
						return new BindingExpr(identifier, identifier);
					}

					@Override
					public CoreExpr binaryOp(BinaryOp op) {
						switch (op.getOperator()) {
						case ASSIGNMENT:
							return assignmentBinding(op.getRight(), op.getLeft());
						default:
							return fallback(op);
						}
					}

					@Override
					public CoreExpr fallback(SourceExpr other) {
						return new BadCoreExpr(other.getRanges(), "Expected identifier / assignment");
					}
				});
			}

			@Override
			public CoreExpr identifier(Identifier name) {
				return new BindingExpr(name, expr(value));
			}

			@Override
			public CoreExpr fallback(SourceExpr other) {
				return new BadCoreExpr(other.getRanges(), "Expected identifier / lvalue");
			}
		});
	}

	public BindingExpr functionShorthand(SourceExpr letOp, SourceExpr name, SourceExpr args, SourceExpr body) {
		return localFunctionDef(letOp, name, args, expr(body), Option.none());
	}

	public BindingExpr localFunctionDef(SourceExpr letOp, SourceExpr name, SourceExpr args, CoreExpr body,
			Option<Identifier> nameSuffix) {
		return name.acceptVisitor(new BaseSourceExprVisitor<BindingExpr>() {
			public BindingExpr function(Identifier name) {
				Identifier fullName = concatNameParts(name, nameSuffix);
				return new BindingExpr(fullName, func(Option.some(fullName)));
			}

			protected CoreExpr func(final Option<SourceExpr> recursiveNameBinding) {
				return functionLiteral(letOp, args, body, recursiveNameBinding);
			}

			@Override
			public BindingExpr binaryOp(BinaryOp op) {
				if (op.getOperator() == Operator.JUXTAPOSITION) {
					Identifier rightName = expectIdentifier(op.getRight());
					return op.getLeft().acceptVisitor(new BaseSourceExprVisitor<BindingExpr>() {
						@Override
						public BindingExpr binaryOp(BinaryOp leftOp) {
							if (leftOp.getOperator() == Operator.CALL) {
								return localFunctionDef(letOp, leftOp.getLeft(), leftOp.getRight(), func(Option.none()),
										Option.some(rightName));
							}
							return fallback(leftOp);
						}

						@Override
						public BindingExpr fallback(SourceExpr other) {
							return new BindingExpr(Identifier.UNDERSCORE,
									new BadCoreExpr(other.getRanges(), "Expected function signature part"));
						}
					});
				}
				return fallback(op);
			}

			@Override
			public BindingExpr identifier(Identifier identifier) {
				return function(identifier);
			}

			@Override
			public BindingExpr fallback(SourceExpr other) {
				return identifier(expectIdentifier(other));
			}
		});
	}

	private CoreExpr juxtaposition(final BinaryOp op) {
		return op.getRight().acceptVisitor(new BaseSourceExprVisitor<CoreExpr>() {
			private CoreExpr nonMixfixCall() {
				return SourceExprToCoreExpr.this.call(op);
			}

			@Override
			public CoreExpr fallback(SourceExpr other) {
				return nonMixfixCall();
			}

			@Override
			public CoreExpr identifier(Identifier keyOnRight) {
				return op.getLeft().acceptVisitor(new SourceExprVisitor<CoreExpr>() {
					private CoreExpr makeMixfixCall(final BinaryOp opLeft) {
						return addNamePartToCall(call(opLeft), keyOnRight);
					}

					@Override
					public CoreExpr unaryOp(UnaryOp opLeft) {
						if (opLeft.getOperator() == Operator.PARENS) {
							return infixNameJuxtaposition(opLeft);
						}
						return nonMixfixCall();
					}

					private CoreExpr infixNameJuxtaposition(SourceExpr arg) {
						return binaryOp(new BinaryOp(Operator.CALL, new Identifier(""), arg));
					}

					@Override
					public CoreExpr binaryOp(final BinaryOp opLeft) {
						if (opLeft.getOperator() == Operator.CALL || opLeft.getOperator() == Operator.JUXTAPOSITION) {
							return makeMixfixCall(opLeft);
						} else {
							return nonMixfixCall();
						}
					}

					@Override
					public CoreExpr stringLiteral(StringLiteral x) {
						return infixNameJuxtaposition(x);
					}

					@Override
					public CoreExpr numberLiteral(NumberLiteral x) {
						return infixNameJuxtaposition(x);
					}

					@Override
					public CoreExpr identifier(Identifier identifier) {
						return nonMixfixCall();
					}

					@Override
					public CoreExpr operator(OperatorRef operatorRef) {
						return nonMixfixCall();
					}

					@Override
					public CoreExpr badSourceExpr(BadSourceExpr badSourceExpr) {
						return nonMixfixCall();
					}

					@Override
					public CoreExpr emptyExpr(EmptyExpr emptyExpr) {
						return expr(op.getRight()); // Not sure if this is even
													// possible, but just in
													// case ...
					}

					@Override
					public CoreExpr badIdentifier(BadIdentifier badIdentifier) {
						return nonMixfixCall();
					}

				});
			}

			private CoreExpr addNamePartToCall(CoreExpr value, Identifier keyOnRight) {
				return value.acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {
					@Override
					public CoreExpr identifier(Identifier n) {
						return concatNameParts(n, keyOnRight);
					}

					@Override
					public CoreExpr scoped(ScopedExpr n) {
						return n.getBody().acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {
							@Override
							public CoreExpr identifier(Identifier slotName) {
								if (slotName.id.equals(Identifier.BETA_REDUCTION.id)) {
									CoreExpr oldTarget = n.getObject();
									CoreExpr newTarget = addNamePartToCall(oldTarget, keyOnRight);

									if (newTarget != oldTarget)
										return new ScopedExpr(n.getRanges(), newTarget, n.getBody(), n.getArgs());
								}
								return new ScopedExpr(n.getRanges(), n.getObject(),
										concatNameParts(slotName, keyOnRight), n.getArgs());
							}

							@Override
							public CoreExpr fallback() {
								return new BadCoreExpr(keyOnRight.getRanges(), "Dangling function name part %s",
										keyOnRight.id);
							}
						});
					}

					@Override
					public CoreExpr fallback() {
						return new BadCoreExpr(keyOnRight.getRanges(), "Dangling function name part %s", keyOnRight.id);
					}
				});
			}

		});
	}

	/**
	 * Implement a unary op as a slot on the operand.
	 */
	private CoreExpr unaryOpToSlotReference(final UnaryOp op) {
		return new ScopedExpr(op.getRanges(), expr(op.getOperand()), opMethodName(op.getOperator()));
	}

	/**
	 * Implement a unary op using a unary function
	 */
	CoreExpr unaryOpToFunctionCall(UnaryOp op) {
		Identifier functionName = opMethodName(op.getOperator());
		final CoreExpr operandCoreExpr = expr(op.getOperand());
		return ScopedExpr.call1(op.getRanges(), functionName, operandCoreExpr);
	}

	/**
	 * List all files at the given path which might be a source file.
	 */
	public static Stream<Path> listSourceFiles(Path path) {
		try {
			return Files.list(path).filter(p -> p.getFileName().toString().charAt(0) != '.')
					.sorted(new Comparator<Path>() {
						@Override
						public int compare(Path o1, Path o2) {
							int cmp = -Boolean.compare(Files.isDirectory(o1), Files.isDirectory(o2));
							if (cmp != 0)
								return cmp;

							for (Iterator<Path> i1 = o1.iterator(), i2 = o2.iterator(); i1.hasNext() || i2.hasNext();) {
								if (!i1.hasNext())
									return 1; // i2 has more elements
								if (!i2.hasNext())
									return -1; // i1 has more elements
								final String s1 = i1.next().toString();
								final String s2 = i2.next().toString();
								cmp = s1.compareToIgnoreCase(s2);
								if (cmp != 0)
									return cmp;
								cmp = s1.compareTo(s2);
								if (cmp != 0)
									return cmp;
							}
							return cmp;
						}
					});
		} catch (IOException e) {
			return Stream.empty();
		}
	}

	/**
	 * Desugar a file path into a slot. The self-reference may be taken from the
	 * filename, if present.
	 */
	public CoreExpr pathToSlot(Path p) {
		SourceExprFactory parser = new SourceExprFactory(p.resolveSibling(p.getFileName() + "(filename)"));
		String slotLhs = p.getFileName().toString();
		SourceExpr lhs;
		boolean directory = Files.isDirectory(p);
		try {
			// If it's not a folder, strip the file extension
			if (!directory) {
				slotLhs = slotLhs.replaceFirst("\\.[^.]*$", "");
			}
			// Also URL-decode to allow illegal filename characters to be used
			// sometimes. Avoid replacing "+" with " ", though.
			try {
				slotLhs = PathUtils.decodeFilename(slotLhs);
			} catch (IllegalArgumentException ex) {
				// Ignore here, should show up as a syntax error later
			}

			lhs = parser.parse(slotLhs);
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}

		CoreExpr value = loadFromPath(p);
		return value.acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {
			@Override
			public CoreExpr nil() {
				// If the body is empty, then the filename is the entire definition
				return expr(lhs);
			}

			@Override
			public CoreExpr fallback() {
				return bindingElement(lhs, lhs, value, Identifier.TRUE, Operator.ASSIGNMENT);
			}
		});
	}

	/**
	 * Create a new slot which contains the old slot value extended with the new
	 * one.
	 * 
	 * The returned binding has the same name as the first binding and its body is
	 * the body of the first extended via object composition with the body of the
	 * second.
	 */
	static BindingExpr mergeSlots(BindingExpr base, BindingExpr extension) {
		CoreExpr b = base.body;
		CoreExpr e = extension.body;
		return new BindingExpr(base.name, new Extend(b, e));
	}

	/**
	 * Inspect a path to produce a CoreExpr. If the target path is a directory /
	 * folder this desugars into an object literal representation of the folder. If
	 * it is a regular file, the file extension is used to determine the format of
	 * the file.
	 */
	public CoreExpr loadFromPath(Path path) {
		if (Files.isDirectory(path)) {
			return loadFromDirectories(List.single(path));
		}
		if (Files.isRegularFile(path)) {
			return loadFromFile(path);
		}
		return new BadCoreExpr(new SourceFileRange(path, FileRange.EMPTY), "Unsupported file type / extension at %s",
				path);
	}

	/**
	 * Desugar a file into a directory.
	 */
	public CoreExpr loadFromFile(Path path) {
		String filename = path.getFileName().toString();
		int dot = filename.lastIndexOf('.');
		String ext = (dot > 0 ? filename.substring(dot + 1) : "");
		if ("txt".equals(ext)) {
			return loadTxtFile(path);
		} else if ("banjo".equals(ext)) {
			return CoreExprFromFile.forPath(path);
		} else {
			// Don't know what to do with this type of file
			return new BadCoreExpr(new SourceFileRange(path, FileRange.EMPTY),
					"Unsupported file type / extension at %s", path);
		}
	}

	/**
	 * Load a text file as a StringLiteral. If the path fails to load for any
	 * reason, returns a BadCoreExpr.
	 */
	public CoreExpr loadTxtFile(Path path) {
		try {
			return new StringLiteral(new String(Files.readAllBytes(path), StandardCharsets.UTF_8));
		} catch (IOException e) {
			return new BadCoreExpr(new SourceFileRange(path, FileRange.EMPTY), "Failed to read text from %s: %s",
					path.toString(), e.toString());
		}
	}

	/**
	 * Load a directory search path into an AST. Note that the AST for individual
	 * files may be loaded lazily later on demand.
	 */
	public CoreExpr loadFromDirectories(List<Path> paths) {
		Stream<Path> slotFiles = StreamSupport.stream(paths.spliterator(), false)
				.flatMap(SourceExprToCoreExpr::listSourceFiles).map(PathUtils::zipFileToZipPath);
		Stream<CoreExpr> slots = slotFiles.map(this::pathToSlot);
		return Extend.composeSlots(List.fromIterator(slots.iterator()));
	}

	/**
	 * Given the a source path somewhere in a project, load the AST for the whole
	 * project.
	 * 
	 * @param sourceFilePath Path to look for the project at
	 * @return AST for the project
	 */
	public CoreExpr loadProjectAstForSourcePath(Path sourceFilePath) {
		List<Path> rootPaths = projectSourcePathsForFile(sourceFilePath.toAbsolutePath());
		// Construct the project root object
		CoreExpr projectAst = loadFromDirectories(rootPaths);
		return projectAst;
	}

	/**
	 * If we find a table heading we can process all the row into objects using the
	 * headings on this row as slot names, and the values on subsequent rows as the
	 * slot bodies.
	 * 
	 * @param unprocessedRows Rows to process using the table headings
	 * @param processedRows   Rows that were already processed, the new rows are
	 *                        added to this list
	 * @param op              UnaryOp representing the table header
	 * @return
	 */
	private P2<List<SourceExpr>, List<CoreExpr>> table(final List<SourceExpr> unprocessedRows,
			final List<CoreExpr> processedRows, UnaryOp op) {
		final List<SourceExpr> headings = flattenCommas(op.getOperand());
		final List<CoreExpr> rows = unprocessedRows.map(row -> makeRow(row, headings)).append(processedRows);
		return P.p(List.nil(), rows);
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
		while (tryPath != null) {
			if (Files.exists(tryPath.resolve(".banjo")))
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
		List<Path> banjoPathPaths = PathUtils
				.pathsFromSearchPath(System.getProperty(SourceExprToCoreExpr.LIB_PATH_SYS_PROPERTY, ""));
		List<Path> classPathPaths = PathUtils.pathsFromSearchPath(System.getProperty("java.class.path", ""))
				.filter(p -> Files.exists(p.resolve(".banjo")));
		return banjoPathPaths.append(classPathPaths);
	}

	/**
	 * Find the full project source search path list for the given source file; this
	 * includes the project the file is in plus the core library search paths.
	 */
	public static List<Path> projectSourcePathsForFile(Path sourceFile) {
		Option<Path> projectRoot = SourceExprToCoreExpr.projectRootForPath(sourceFile);
		List<Path> coreLibraryPaths = SourceExprToCoreExpr.getGlobalSourcePaths();
		return coreLibraryPaths.append(projectRoot.toList());
	}

	public static void main(String[] args) {
		ArrayList<Path> paths = new ArrayList<Path>();
		boolean printSourcePath = false;
		boolean verbose = true;
		for (String arg : args) {
			if (arg.startsWith("--")) {
				if ("--print-source-path".equals(arg)) {
					printSourcePath = true;
				} else {
					System.err.println("Unknown option: " + arg);
					return;
				}
			} else if (arg.startsWith("-")) {
				for (char ch : arg.substring(1).toCharArray()) {
					switch (ch) {
					case 'v':
						verbose = true;
						break;
					default:
						System.err.println("Unknown option: -" + String.valueOf(ch));
						return;
					}
				}
			} else {
				paths.add(Paths.get(arg));
			}
		}

		if (verbose) {
			System.out.println("Verbose is on");
		}
		if (paths.isEmpty())
			paths.add(Paths.get("."));
		for (Path path : paths) {
			if (printSourcePath) {
				System.out.println("java.class.path == " + System.getProperty("java.class.path", "<not set>"));
				System.out.println("source paths(" + StringLiteral.toSource(path.toString()) + ") == [");
				for (Path p : projectSourcePathsForFile(path)) {
					System.out.print("  ");
					System.out.println(p.toUri());
				}
				System.out.println("]");
			}
		}
	}
}
