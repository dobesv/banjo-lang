package banjo.desugar;

import static banjo.parser.util.Check.nonNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.desugar.errors.ExpectedField;
import banjo.desugar.errors.ExtraTableColumn;
import banjo.desugar.errors.MissingValueForTableColumn;
import banjo.desugar.errors.UnexpectedEllipsis;
import banjo.dom.core.BadExpr;
import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.ExprList;
import banjo.dom.core.FunArg;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.Let;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.Method;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.core.OffsetCoreExpr;
import banjo.dom.core.Projection;
import banjo.dom.core.SetLiteral;
import banjo.dom.source.BadSourceExpr;
import banjo.dom.source.BaseSourceExprVisitor;
import banjo.dom.source.BinaryOp;
import banjo.dom.source.EmptyExpr;
import banjo.dom.source.OffsetSourceExpr;
import banjo.dom.source.Operator;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.SourceExprVisitor;
import banjo.dom.source.UnaryOp;
import banjo.dom.token.Ellipsis;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OffsetKey;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.parser.errors.ExpectedElement;
import banjo.parser.errors.ExpectedExpression;
import banjo.parser.errors.ExpectedIdentifier;
import banjo.parser.errors.Problem;
import banjo.parser.errors.UnsupportedBinaryOperator;
import banjo.parser.errors.UnsupportedUnaryOperator;
import banjo.parser.util.ListUtil;
import banjo.parser.util.Problematic;

public class BanjoDesugarer {
	protected final LinkedHashMap<SourceExpr,Problematic<CoreExpr>> cache = new LinkedHashMap<>();

	/**
	 * Coming in we have a tree of basically just unary and binary operations and parens and atoms.
	 * 
	 * Desugaring changes that into function calls, projections, etc..
	 */
	public Problematic<CoreExpr> desugar(SourceExpr node) {
		return expr(node, 0);
	}

	public Problematic<CoreExpr> expr(OffsetSourceExpr node, int parentSourceOffset) {
		final SourceExpr sourceExpr = node.getValue();
		final int sourceOffset = node.getOffset() + parentSourceOffset;
		return expr(sourceExpr, sourceOffset);
	}

	public Problematic<CoreExpr> expr(SourceExpr sourceExpr, int sourceOffset) {
		// TODO When fetching from the cache, the offsets in the errors might be a off ... problem!
		final Problematic<CoreExpr> cachedResult = this.cache.get(sourceExpr);
		if(cachedResult != null) {
			return cachedResult;
		}
		final Problematic<CoreExpr> result = nonNull(sourceExpr.acceptVisitor(new DesugarVisitor(sourceOffset)));
		this.cache.put(sourceExpr, result);
		return result;
	}

	private Problematic<CoreExpr> comparison(BinaryOp op, int sourceOffset) {
		final SourceExpr leftSourceExpr = op.getLeft();
		final SourceExpr rightSourceExpr = op.getRight();
		final Problematic<CoreExpr> leftCoreExpr = expr(leftSourceExpr, sourceOffset);
		final Problematic<CoreExpr> rightCoreExpr = expr(rightSourceExpr, sourceOffset);
		final Projection fieldRef = new Projection(op, off(leftSourceExpr, leftCoreExpr), new Identifier(nonNull(Operator.CMP.getOp())));
		final CoreExpr cmp = new Call(op, fieldRef, off(rightSourceExpr, rightCoreExpr));
		boolean checkEqual;
		String checkField;
		switch(op.getOperator()) {
		case GT: checkEqual = true; checkField = ">"; break;
		case GE: checkEqual = false; checkField = "<"; break;
		case LT: checkEqual = true; checkField = "<"; break;
		case LE: checkEqual = false; checkField = ">"; break;
		default: throw new Error();
		}
		final Projection cmpFieldRef = new Projection(op, cmp, new Identifier(checkField));
		CoreExpr check = new Call(op, cmpFieldRef);
		if(!checkEqual)
			check = new Call(op, new Projection(op, check, new Identifier(nonNull(Operator.NOT.getOp()))));
		return result(check, ListUtil.concat(leftCoreExpr.getProblems(), rightCoreExpr.getProblems()));
	}

	/**
	 * Projection.
	 * 
	 * @see Projection
	 * @param op BinaryOp describing the projection (having PROJECTION as its operator)
	 * @param sourceOffset Source offset to the start of op
	 */
	private Problematic<CoreExpr> projection(BinaryOp op, int sourceOffset) {
		return projection(op, sourceOffset, op.getLeft(), sourceOffset + op.getLeft().getOffsetInParent(), op.getRight(), sourceOffset + op.getRight().getOffsetInParent());
	}

	/**
	 * Create a projection.
	 * 
	 * @param sourceExpr Root source expression for the projection.
	 * @param sourceOffset Absolute file offset of the start of the expression described by op
	 * @param baseCoreExpr Desugared left-hand side of the projection; the "base" object we're projecting from
	 * @param objectSourceOffset Absolute file offset of the start of the left-hand side of the projection
	 * @param projectionCoreExpr Desugared right-hand side of the project; the description of the projection
	 * @param exprSourceOffset Absolute file offset of the right-hand side of the projection
	 * @return A new CoreExpr, possibly with errors
	 */
	private Problematic<CoreExpr> projection(final SourceExpr sourceExpr, final int sourceOffset, final SourceExpr objectExpr, final int objectSourceOffset, final SourceExpr expr, final int exprSourceOffset) {
		final Problematic<CoreExpr> object = expr(objectExpr, sourceOffset);
		final Problematic<CoreExpr> exprCoreExpr = expr(expr, sourceOffset);
		return result(new Projection(sourceExpr,
				off(objectSourceOffset - sourceOffset, object.getValue()),
				off(exprSourceOffset - sourceOffset, exprCoreExpr.getValue())), object, exprCoreExpr);
	}

	/**
	 * Optional projection is a projection that works on an option type - it gives a
	 * new option value which is the result of the projection.  The right-hand side
	 * is only evaluated if the option value is present.
	 * 
	 * <pre> x?.foo == x.map((x) -> x.foo)</pre>
	 */
	private Problematic<CoreExpr> optionProjection(BinaryOp op, int sourceOffset) {
		final Key argName = gensym();
		final Problematic<CoreExpr> projection = projection(op, sourceOffset, argName, sourceOffset, op.getRight(), sourceOffset + op.getRight().getOffsetInParent());
		final CoreExpr projectFunc = new FunctionLiteral(op, new FunArg(op.getSourceLength(), argName), projection.getValue());
		final Problematic<CoreExpr> left = expr(op.getLeft(), sourceOffset);
		final CoreExpr mapCall = new Call(op, new Projection(op, off(op.getLeft(), left), new Identifier("map")), projectFunc);
		return result(mapCall, projection.getProblems());
	}

	private static Key off(int offset, Key key) {
		if(offset == 0) return key;
		return new OffsetKey(offset, key);
	}
	private static CoreExpr off(SourceExpr sourceExpr, CoreExpr e) {
		return off(sourceExpr.getOffsetInParent(), e);
	}

	private static CoreExpr off(SourceExpr sourceExpr, Problematic<CoreExpr> e) {
		return off(sourceExpr, e.getValue());
	}

	private static CoreExpr off(int offset, Problematic<CoreExpr> e) {
		return off(offset, e.getValue());
	}

	private static CoreExpr off(int offset, CoreExpr value) {
		if(offset == 0) return value;
		else return new OffsetCoreExpr(offset, value);
	}

	private static List<CoreExpr> offAll(SourceExpr sourceExpr, Collection<CoreExpr> exprs) {
		return offAll(sourceExpr.getOffsetInParent(), exprs);
	}

	private static SourceExpr off(int offset, SourceExpr value) {
		if(offset == 0) return value;
		else return new OffsetSourceExpr(offset, value);
	}


	private static List<CoreExpr> offAll(int offsetInParent, Collection<CoreExpr> exprs) {
		final ArrayList<CoreExpr> result = new ArrayList<>(exprs.size());
		for(final CoreExpr e : exprs) {
			result.add(off(offsetInParent, nonNull(e)));
		}
		return result;
	}



	int gensymCounter = 0;
	private Key gensym() {
		this.gensymCounter++;
		return new Identifier("__t"+this.gensymCounter);
	}

	private Problematic<CoreExpr> call(BinaryOp op, int sourceOffset) {
		final Problematic<CoreExpr> callee = expr(op.getLeft(), sourceOffset);
		final Problematic<CoreExpr> arg = expr(op.getRight(), sourceOffset);
		final int argsOffset = op.getRight().getOffsetInParent();
		final List<CoreExpr> args = arg.getValue().acceptVisitor(new BaseCoreExprVisitor<List<CoreExpr>>() {
			@Override
			@NonNull
			public List<CoreExpr> exprList(ExprList n) {
				final List<CoreExpr> elts = n.getElements();
				final List<CoreExpr> args = new ArrayList<>(elts.size());
				for(final CoreExpr elt : elts) {
					// Adjust the offset in parent so it's relative to the whole call, not just the argument list
					args.add(off(argsOffset, nonNull(elt)));
				}
				return args;
			}

			@Override
			@NonNull
			public List<CoreExpr> fallback(CoreExpr other) {
				// Adjust the offset in parent so it's relative to the whole call, not just the argument list
				return nonNull(Collections.singletonList(off(argsOffset, arg)));
			}
		});
		return result(new Call(op, off(op.getLeft(), callee), nonNull(args)), ListUtil.concat(callee.getProblems(), arg.getProblems()));
	}

	private Problematic<CoreExpr> let(final BinaryOp op, final int sourceOffset) {
		final int leftSourceOffset = sourceOffset + op.getLeft().getOffsetInParent();
		final int rightSourceOffset = sourceOffset + op.getRight().getOffsetInParent();
		final Problematic<CoreExpr> rightCoreExpr = expr(op.getRight(), rightSourceOffset);

		return let(op, sourceOffset, op.getLeft(), leftSourceOffset, rightCoreExpr.getValue(), rightSourceOffset).plusProblemsFrom(rightCoreExpr);
		//		if(isPair(targetOffsetExpr)) {
		//		}
		//
		//		Problematic<CoreExpr> valueCoreExpr = expr(op.getRight(), sourceOffset);
		//		problems.addAll(valueCoreExpr.getProblems());
		//		final int dsValueOffset = op.getRight().getOffset();
		//		if(isCall(targetOffsetExpr.getValue())) {
		//		}
		//		OffsetKey name;
		//		if(!(targetOffsetExpr.getValue() instanceof Key)) {
		//			name = new OffsetKey(targetOffsetExpr.getOffset(), new Identifier(targetOffsetExpr.getSourceLength(), targetOffsetExpr.toSource()));
		//			problems.add(new ExpectedIdentifier(targetOffsetExpr.getValue(), sourceOffset + targetOffsetExpr.getOffset(), targetOffsetExpr.getSourceLength()));
		//		} else {
		//			name = new OffsetKey(targetOffsetExpr.getOffset(), (Key)targetOffsetExpr.getValue());
		//		}
		//		// TODO If there was a contract specified, maybe we should check it here ... or maybe report an error ... kind of an odd case
		//		//if(!(guarantee.getValue().equals(FunctionLiteral.NO_GUARANTEE))) {
		//		// Error: contract not expected here
		//		//}
		//		return result(new Let(op, name, off(dsValueOffset,valueCoreExpr)), problems);
	}

	public Problematic<CoreExpr> let(final BinaryOp sourceExpr, final int sourceOffset,
			final SourceExpr left, final int leftSourceOffset,
			final CoreExpr right, final int rightSourceOffset) {
		return nonNull(left.acceptVisitor(new BaseSourceExprVisitor<Problematic<CoreExpr>>() {
			@Override
			@Nullable
			public Problematic<CoreExpr> binaryOp(BinaryOp targetBOp) {
				switch(targetBOp.getOperator()) {
				case PAIR: {

					final int guaranteeSourceOffset = leftSourceOffset + targetBOp.getRight().getOffsetInParent();
					final Problematic<CoreExpr> guarantee = expr(targetBOp.getRight(), guaranteeSourceOffset);
					final SourceExpr newLeft = targetBOp.getLeft();
					final int newLeftSourceOffset = leftSourceOffset + newLeft.getOffsetInParent();
					return let(sourceExpr, sourceOffset, newLeft, newLeftSourceOffset, right, rightSourceOffset, guarantee.getValue(), guaranteeSourceOffset).plusProblemsFrom(guarantee);
				}

				default: return fallback(targetBOp);
				}
			}

			@Override
			@Nullable
			public Problematic<CoreExpr> unaryOp(UnaryOp target) {
				switch(sourceExpr.getOperator()) {
				case PARENS: { // Need parens if it's a method with a guarantee on it, like (foo() : Bar) : ...
					final SourceExpr newLeft = target.getOperand();
					final int newLeftSourceOffset = leftSourceOffset + newLeft.getOffsetInParent();
					return let(sourceExpr, sourceOffset, newLeft, newLeftSourceOffset, right, rightSourceOffset);
				}
				default:
					return fallback(target);
				}
			}

			@Override
			@Nullable
			public Problematic<CoreExpr> fallback(SourceExpr other) {
				return let(sourceExpr, sourceOffset, left, leftSourceOffset, right, rightSourceOffset, FunctionLiteral.DEFAULT_GUARANTEE, sourceOffset);
			}
		}));
	}

	protected Problematic<CoreExpr> let(
			final BinaryOp sourceExpr, final int sourceOffset,
			final SourceExpr left,    final int leftSourceOffset,
			final CoreExpr right,     final int rightSourceOffset,
			final CoreExpr guarantee, final int guaranteeSourceOffset) {
		return nonNull(left.acceptVisitor(new BaseSourceExprVisitor<Problematic<CoreExpr>>() {
			@Override
			@Nullable
			public Problematic<CoreExpr> binaryOp(BinaryOp targetBOp) {
				switch(targetBOp.getOperator()) {
				case CALL: {
					final SourceExpr args = targetBOp.getRight();
					final int argsSourceOffset = leftSourceOffset + args.getOffsetInParent();
					final Problematic<CoreExpr> newRight = functionLiteral(
							sourceExpr, sourceOffset,
							args, argsSourceOffset,
							guarantee, guaranteeSourceOffset,
							right, rightSourceOffset);
					final int newRightSourceOffset = sourceOffset;
					final SourceExpr newLeft = targetBOp.getLeft();
					final int newLeftSourceOffset = leftSourceOffset + newLeft.getOffsetInParent();
					return let(sourceExpr, sourceOffset, newLeft, newLeftSourceOffset, newRight.getValue(), newRightSourceOffset).plusProblemsFrom(newRight);
				}
				default: return fallback(targetBOp);
				}
			}

			@Override
			public Problematic<CoreExpr> key(Key name) {
				// TODO If guarantee is not the default, apply that guarantee ...
				return result(
						new Let(sourceExpr,
								off(leftSourceOffset - sourceOffset, name),
								off(rightSourceOffset - sourceOffset, right)));
			}

			@Override
			@Nullable
			public Problematic<CoreExpr> fallback(SourceExpr other) {
				final Problem problem = new ExpectedIdentifier(other, leftSourceOffset, other.getSourceLength());
				return key(new Identifier(other.getSourceLength(), other.toSource())).with(problem);
			}

		}));
	}

	/**
	 * For a conditional each "=>" binary operator can yield an Option-like value.  Conditions are
	 * chained using implicit right-associative "?:" between them ("or else")
	 * 
	 * a => b
	 * means
	 * a."=>"(-> b)
	 * means
	 * a.if(true:some(b) false:none)
	 * 
	 * a => b ; c => d ; e
	 * means
	 * a => b ?: c => d ?: e
	 * 
	 * @param args List of cond expressions, each with offsetInParent relative to the given sourceOffset
	 */
	private Problematic<CoreExpr> exprListToCond(SourceExpr op, int sourceOffset, LinkedList<SourceExpr> exprs) {
		if(exprs.isEmpty()) throw new IllegalStateException();

		final LinkedList<Problem> problems = new LinkedList<>();

		final LinkedList<CoreExpr> coreExprs = new LinkedList<>();
		for(final SourceExpr se : exprs) {
			coreExprs.add(off(se.getOffsetInParent(), expr(se, sourceOffset + se.getOffsetInParent()).dumpProblems(problems)));
		}

		// Now insert ?: between each
		final Iterator<CoreExpr> it = coreExprs.descendingIterator();
		CoreExpr result = nonNull(it.next());
		while(it.hasNext()) {
			final CoreExpr cond = nonNull(it.next());
			result = new Call(0, new Projection(0, cond, new Identifier(Operator.LAZY_OR.getOp())), nonNull(Collections.singletonList(result)));
		}
		return result(result, problems);
	}

	private Problematic<CoreExpr> listLiteral(SourceExpr sourceExpr, int sourceOffset, List<SourceExpr> list, @Nullable Operator requireBullet) {
		final List<Problem> problems = new ArrayList<>();
		return result(new ListLiteral(sourceExpr, elements(sourceOffset, list, requireBullet, problems)), problems);
	}

	private Problematic<CoreExpr> setLiteral(SourceExpr sourceExpr, int sourceOffset, List<SourceExpr> list, @Nullable Operator requireBullet) {
		final List<Problem> problems = new ArrayList<>();
		return result(new SetLiteral(sourceExpr, elements(sourceOffset, list, requireBullet, problems)), problems);
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
	private ArrayList<CoreExpr> elements(final int sourceOffset, List<SourceExpr> list, @Nullable final Operator requireBullet, final List<Problem> problems) {
		final ArrayList<CoreExpr> elements = new ArrayList<>(list.size());
		if(!list.isEmpty()) {
			final ArrayList<SourceExpr> headings = new ArrayList<>();
			for(final SourceExpr e : list) {
				if(e == null) throw new NullPointerException();
				final int elementSourceOffset = sourceOffset + e.getOffsetInParent();
				e.acceptVisitor(new BaseSourceExprVisitor<Void>() {
					@Override
					@Nullable
					public Void unaryOp(UnaryOp op) {
						if(op.getOperator() == Operator.TABLE_HEADER) {
							headings.clear();
							// Table headings here will end up each with an offset relative to the list itself, since they have no parent
							final int headingsOffsetFromList = e.getOffsetInParent() + op.getOperand().getOffsetInParent();
							flattenCommas(op.getOperand(), headingsOffsetFromList, headings);
							return null;
						} else {
							if(requireBullet != null) {
								if(op.getOperator() == requireBullet) {
									return visitElement(elementSourceOffset + op.getOperand().getOffsetInParent(), op.getOperand());
								} else {
									problems.add(new ExpectedElement(elementSourceOffset, e.getSourceLength()));
									return null;
								}
							}
							return visitElement(elementSourceOffset, op);
						}
					}

					@Nullable
					public Void visitElement(int elementSourceOffset, SourceExpr eltSourceExpr) {
						final int bulletOffset = 0;
						Problematic<CoreExpr> coreElt;
						if(!headings.isEmpty()) {
							coreElt = makeRow(eltSourceExpr, sourceOffset, headings, sourceOffset);
						} else {
							coreElt = expr(eltSourceExpr, sourceOffset + bulletOffset);
						}
						elements.add(off(elementSourceOffset - sourceOffset, coreElt));
						problems.addAll(coreElt.getProblems());
						return null;
					}

					@Override
					@Nullable
					public Void fallback(SourceExpr other) {
						if(requireBullet != null) {
							problems.add(new ExpectedElement(sourceOffset + e.getOffsetInParent(), e.getSourceLength()));
						} else {
							visitElement(elementSourceOffset, other);
						}
						return null;
					}
				});
			}
		}
		return elements;
	}

	private Problematic<CoreExpr> objectLiteral(SourceExpr sourceExpr, final int sourceOffset, Collection<SourceExpr> fieldSourceExprs) {
		// Key/value pair - treat as an object
		final LinkedHashMap<String, Method> fields = new LinkedHashMap<>(fieldSourceExprs.size()*2);
		final ArrayList<SourceExpr> headings = new ArrayList<>();
		final List<Problem> problems = new ArrayList<>();
		for(final SourceExpr fieldSourceExpr : fieldSourceExprs) {
			if(fieldSourceExpr == null) throw new NullPointerException();
			final int fieldSourceOffset = sourceOffset + fieldSourceExpr.getOffsetInParent();
			addField(sourceOffset, fieldSourceExpr, fieldSourceOffset, headings, fields, problems);
		}
		return result(new ObjectLiteral(sourceExpr, fields), problems);
	}

	protected void addField(final int objectSourceOffset,
			final SourceExpr fieldSourceExpr, final int fieldSourceOffset,
			final ArrayList<SourceExpr> headings,
			final LinkedHashMap<String, Method> fields,
			final List<Problem> problems) {
		fieldSourceExpr.acceptVisitor(new BaseSourceExprVisitor<Void>() {
			@Override
			@Nullable
			public Void binaryOp(BinaryOp op) {
				if(op.getOperator() == Operator.PAIR) {
					return visitPair(op);
				} else {
					return fallback(op);
				}
			}

			@Nullable
			private Void visitPair(BinaryOp fieldOp) {
				final SourceExpr left = fieldOp.getLeft();
				final SourceExpr right = fieldOp.getRight();
				return pair(
						left,  fieldSourceOffset + left.getOffsetInParent(),
						right, fieldSourceOffset + right.getOffsetInParent(),
						fieldOp.getOperator() == Operator.PAIR_INCLUDE);
			}

			@Override
			@Nullable
			public Void unaryOp(UnaryOp op) {
				switch(op.getOperator()) {
				case ANON_INCLUDE: return visitAnonInclude(op);
				case MIRROR: return visitMirror(op);
				case TABLE_HEADER: return visitTableHeader(op);
				default: return fallback(op);
				}
			}

			@Nullable
			private Void visitTableHeader(UnaryOp headerOp) {
				headings.clear();
				final int headingsOffsetFromObject = fieldSourceExpr.getOffsetInParent() + headerOp.getOperand().getOffsetInParent();
				flattenCommas(headerOp.getOperand(), headingsOffsetFromObject, headings);
				return null;
			}

			@Nullable
			private Void visitMirror(UnaryOp fieldOp) {
				final SourceExpr operand = fieldOp.getOperand();
				final int operandSourceOffset = fieldSourceOffset + operand.getOffsetInParent();
				return pair(operand, operandSourceOffset, operand, operandSourceOffset, fieldOp.getOperator() == Operator.ANON_INCLUDE);
			}

			@Nullable
			private Void visitAnonInclude(UnaryOp includeOp) {
				final SourceExpr operand = includeOp.getOperand();
				final int operandSourceOffset = fieldSourceOffset + operand.getOffsetInParent();
				final Key fakeName = gensym();
				return pair(fakeName, operandSourceOffset, operand, operandSourceOffset, true);
			}

			@Nullable
			private Void pair(SourceExpr lvalueExpr, int lvalueSourceOffset, SourceExpr valueSourceExpr, int valueSourceOffset, boolean include) {
				final CoreExpr valueCoreExpr = element(valueSourceExpr, valueSourceOffset, headings, objectSourceOffset).dumpProblems(problems);
				addField(objectSourceOffset,
						fieldSourceExpr, fieldSourceOffset,
						lvalueExpr, lvalueSourceOffset,
						valueCoreExpr, valueSourceOffset,
						FunctionLiteral.DEFAULT_GUARANTEE, fieldSourceOffset,
						include, fields, problems);
				return null;
			}


			@Override
			@Nullable
			public Void fallback(SourceExpr other) {
				problems.add(new ExpectedField(fieldSourceOffset, fieldSourceExpr.getSourceLength()));
				return null;
			}
		});
	}

	protected Problematic<CoreExpr> element(SourceExpr sourceExpr,
			int sourceOffset, ArrayList<SourceExpr> headings, int headingsSourceOffset) {
		if(headings.isEmpty()) {
			return expr(sourceExpr, sourceOffset);
		} else {
			return makeRow(sourceExpr, sourceOffset, headings, headingsSourceOffset);
		}
	}

	/**
	 * Add a field to the table.  This parses the given nodes and adds an element to the given fields map, and
	 * adds any problems that were found to the given problems list.
	 * 
	 * Each node's offset should be relative to the given parentSourceOffset.
	 * @param include TODO
	 */
	private void addField(final int objectSourceOffset,
			final SourceExpr fieldSourceExpr, final int fieldSourceOffset,
			final SourceExpr lvalueExpr,      final int lvalueSourceOffset,
			final CoreExpr valueExpr,         final int valueSourceOffset,
			final CoreExpr guarantee,         final int guaranteeSourceOffset,
			final boolean include,
			final LinkedHashMap<String, Method> fields, final List<Problem> problems) {

		lvalueExpr.acceptVisitor(new BaseSourceExprVisitor<Void>() {
			@Override
			@Nullable
			public Void binaryOp(BinaryOp targetBOp) {
				switch(targetBOp.getOperator()) {
				case PAIR: return pair(targetBOp);
				case CALL: return method(targetBOp);
				default: return fallback(targetBOp);
				}
			}

			private @Nullable Void method(final BinaryOp signature) {
				final SourceExpr methodLeftExpr = signature.getLeft();
				final int methodLeftSourceOffset = lvalueSourceOffset + methodLeftExpr.getOffsetInParent();
				final SourceExpr argsExpr = signature.getRight();
				final int argsSourceOffset = lvalueSourceOffset + signature.getRight().getOffsetInParent();
				methodLeftExpr.acceptVisitor(new BaseSourceExprVisitor<Void>() {
					@Override
					@Nullable
					public Void binaryOp(BinaryOp methodDefBOp) {
						switch(methodDefBOp.getOperator()) {
						case PROJECTION: return projection(methodDefBOp);
						default: return fallback(methodDefBOp);
						}
					}

					@Nullable
					private Void projection(BinaryOp methodDefBOp) {
						final SourceExpr nameSourceExpr = methodDefBOp.getRight();
						final int nameSourceOffset = methodLeftSourceOffset + nameSourceExpr.getOffsetInParent();
						final SourceExpr selfNameSourceExpr = methodDefBOp.getLeft();
						final int selfNameSourceOffset = methodLeftSourceOffset + selfNameSourceExpr.getOffsetInParent();
						final Key selfName = expectIdentifier(selfNameSourceExpr, methodLeftSourceOffset, problems);
						apply(nameSourceExpr, nameSourceOffset, selfName, selfNameSourceOffset);
						return null;
					}

					void apply(SourceExpr nameExpr, int nameSourceOffset, Key selfName, int selfNameSourceOffset) {
						final FunctionLiteral implementation1 = (FunctionLiteral)functionLiteral(
								fieldSourceExpr, fieldSourceOffset,
								argsExpr, argsSourceOffset,
								guarantee, guaranteeSourceOffset,
								valueExpr, valueSourceOffset).dumpProblems(problems);
						final Key key = expectIdentifier(nameExpr, nameSourceOffset, problems);
						final List<FunArg> funArgs = ListUtil.prepend(new FunArg(selfName.getSourceLength(), selfName), implementation1.getArgs());
						final CoreExpr implementation = new FunctionLiteral(implementation1.getSourceLength(), funArgs, implementation1.getGuarantee(), implementation1.getBody());
						final int keySourceOffset = lvalueSourceOffset;
						final Method field = new Method(fieldSourceExpr.getSourceLength(),
								fieldSourceOffset - objectSourceOffset,
								off(keySourceOffset - fieldSourceOffset, key),
								off(implementation1.getOffsetInParent(), implementation),
								include);
						fields.put(field.getKey().getKeyString(), field);
					}
					@Override
					@Nullable
					public Void fallback(SourceExpr other) {
						apply(methodLeftExpr, methodLeftSourceOffset, Method.DEFAULT_SELF_NAME, methodLeftSourceOffset);
						return null;
					}
				});
				return null;
			}

			private @Nullable Void pair(BinaryOp targetBOp) {
				final SourceExpr newLvalueExpr = targetBOp.getLeft();
				final int newLvalueSourceOffset = lvalueSourceOffset + newLvalueExpr.getOffsetInParent();
				final int newGuaranteeSourceOffset = lvalueSourceOffset + targetBOp.getRight().getOffsetInParent();
				final CoreExpr newGuarantee = expr(targetBOp.getRight(), newGuaranteeSourceOffset).dumpProblems(problems);
				final CoreExpr combinedGuarantee = composeGuarantees(guarantee, guaranteeSourceOffset, newGuarantee, newGuaranteeSourceOffset);
				addField(objectSourceOffset,
						fieldSourceExpr, fieldSourceOffset,
						newLvalueExpr, newLvalueSourceOffset,
						valueExpr, valueSourceOffset,
						combinedGuarantee, newGuaranteeSourceOffset,
						targetBOp.getOperator() == Operator.PAIR_INCLUDE,
						fields, problems);
				return null;
			}

			@Override
			@Nullable
			public Void fallback(SourceExpr other) {
				final Key selfName = Method.DEFAULT_SELF_NAME;
				final List<FunArg> funArgs = nonNull(Collections.singletonList(new FunArg(selfName.getSourceLength(), selfName)));
				final CoreExpr implementation = new FunctionLiteral(fieldSourceExpr.getSourceLength(), funArgs, guarantee, valueExpr);
				final Key key = expectIdentifier(lvalueExpr, lvalueSourceOffset, problems);
				final int keySourceOffset = lvalueSourceOffset;
				final Method field = new Method(fieldSourceExpr.getSourceLength(),
						fieldSourceOffset - objectSourceOffset,
						off(keySourceOffset - fieldSourceOffset, key),
						off(valueSourceOffset - fieldSourceOffset, implementation), include);
				fields.put(field.getKey().getKeyString(), field);
				return null;
			}
		});

	}

	public Key expectIdentifier(final SourceExpr sourceExpr,
			final int sourceOffset,
			final List<Problem> problems) {
		return nonNull(sourceExpr.acceptVisitor(new BaseSourceExprVisitor<Key>() {
			@Override
			@Nullable
			public Key key(Key key) {
				return key;
			}

			@Override
			@Nullable
			public Key fallback(SourceExpr other) {
				problems.add(new ExpectedIdentifier(other, sourceOffset, other.getSourceLength()));
				return new Identifier(other.getSourceLength(), other.toSource());
			}
		}));
	}

	public Key expectIdentifier(final CoreExpr coreExpr,
			final int sourceOffset,
			final List<Problem> problems) {
		return nonNull(coreExpr.acceptVisitor(new BaseCoreExprVisitor<Key>() {
			@Override
			@Nullable
			public Key key(Key key) {
				return key;
			}

			@Override
			@Nullable
			public Key fallback(CoreExpr other) {
				problems.add(new ExpectedIdentifier(other, sourceOffset, other.getSourceLength()));
				return new Identifier(other.getSourceLength(), other.toSource());
			}
		}));
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
	protected CoreExpr composeGuarantees(CoreExpr guarantee,
			int guaranteeSourceOffset, CoreExpr newGuarantee,
			int newGuaranteeSourceOffset) {
		if(guarantee.equals(FunctionLiteral.DEFAULT_GUARANTEE)) {
			return newGuarantee;
		} else if(newGuarantee.equals(FunctionLiteral.DEFAULT_GUARANTEE)) {
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
	private Problematic<CoreExpr> makeRow(SourceExpr sourceExpr, int sourceOffset, List<SourceExpr> headings, int headingsSourceOffset) {
		final List<SourceExpr> values = flattenCommas(stripParens(sourceExpr), 0, new ArrayList<SourceExpr>(headings.size()));
		final LinkedList<Problem> problems = new LinkedList<>();
		for(int i=headings.size(); i < values.size(); i++) {
			final SourceExpr val = values.get(i);
			final int valSourceOffset = sourceOffset = val.getOffsetInParent();
			problems.add(new ExtraTableColumn(i, headings.size(), val.toSource(), valSourceOffset, val.getSourceLength()));
		}
		if(values.size() < headings.size()) {
			problems.add(new MissingValueForTableColumn(values.size(), headings.size(), sourceOffset + sourceExpr.getSourceLength(), 0, headings.get(values.size()).toSource()));
		}
		final LinkedHashMap<String, Method> fields = new LinkedHashMap<>(headings.size()*2);
		for(int i=0; i < values.size(); i++) {
			final SourceExpr headingExpr = nonNull(headings.get(i));
			final int headingSourceOffset = headingsSourceOffset + headingExpr.getOffsetInParent();
			final SourceExpr cellSourceExpr = nonNull(values.get(i));
			final int cellSourceOffset = sourceOffset + cellSourceExpr.getOffsetInParent();
			final CoreExpr cellCoreExpr = expr(cellSourceExpr, cellSourceOffset).dumpProblems(problems);
			addField(sourceOffset, cellSourceExpr, cellSourceOffset, headingExpr, headingSourceOffset, cellCoreExpr, cellSourceOffset, FunctionLiteral.DEFAULT_GUARANTEE, sourceOffset, false, fields, problems);
		}
		return result(new ObjectLiteral(sourceExpr, fields), problems);
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
					return off(op.getOperand().getOffsetInParent(), op.getOperand());
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
	private Problematic<CoreExpr> functionLiteral(BinaryOp sourceExpr, int sourceOffset) {
		final SourceExpr args = sourceExpr.getLeft();
		final SourceExpr body = sourceExpr.getRight();
		final int bodySourceOffset = sourceOffset + sourceExpr.getRight().getOffsetInParent();
		final Problematic<CoreExpr> bodyCoreExpr = expr(body, bodySourceOffset);
		final int argsSourceOffset = sourceOffset+args.getOffsetInParent();
		return functionLiteral(sourceExpr, sourceOffset,
				args, argsSourceOffset,
				FunctionLiteral.DEFAULT_GUARANTEE, sourceOffset,
				bodyCoreExpr.getValue(), bodySourceOffset).plusProblemsFrom(bodyCoreExpr);
	}

	/**
	 * Create a function literal from an args definition (still in source form) and a body (already desugared).
	 * 
	 * @param sourceExpr Function source expression, covering the whole function definition
	 * @param sourceOffset Absolute source offset of sourceExpr
	 * @param args Argument source expression
	 * @param argsSourceOffset Absolute offset in characters to the args
	 * @param body Function body as a core expression
	 * @param bodySourceOffset Absolute source offset of the function body expression
	 * @param guarantee Function result guarantee.  Use FunctionLiteral.DEFAULT_GUARANTEE if none specified
	 * @param guaranteeSourceOffset Absolute source offset of the guarantee; use the same offset as sourceOffset if none specified
	 */
	private Problematic<CoreExpr> functionLiteral(final SourceExpr sourceExpr, final int sourceOffset,
			final SourceExpr args, final int argsSourceOffset,
			final CoreExpr guarantee, final int guaranteeSourceOffset,
			final CoreExpr body, final int bodySourceOffset) {

		return nonNull(args.acceptVisitor(new BaseSourceExprVisitor<Problematic<CoreExpr>>() {
			@Override
			public Problematic<CoreExpr> binaryOp(BinaryOp op) {
				switch(op.getOperator()) {
				case PAIR:
					// (x,y):Guarantee -> ...
					return pair(op);
				default:
					return fallback(op);
				}
			}

			private Problematic<CoreExpr> pair(BinaryOp op) {
				final SourceExpr newArgs = op.getLeft();
				final int newArgsSourceOffset = argsSourceOffset + newArgs.getOffsetInParent();
				final SourceExpr newGuaranteeSourceExpr = op.getRight();
				final int newGuaranteeSourceOffset = argsSourceOffset + newGuaranteeSourceExpr.getOffsetInParent();
				final Problematic<CoreExpr> newGuarantee = expr(newGuaranteeSourceExpr, newGuaranteeSourceOffset);
				final CoreExpr combinedGuarantee = composeGuarantees(guarantee, guaranteeSourceOffset, newGuarantee.getValue(), newGuaranteeSourceOffset);
				return functionLiteral(sourceExpr, sourceOffset,
						newArgs, newArgsSourceOffset,
						combinedGuarantee, newGuaranteeSourceOffset,
						body, bodySourceOffset).plusProblemsFrom(newGuarantee);
			}

			@Override
			public Problematic<CoreExpr> fallback(SourceExpr other) {
				// Args should be comma-separated
				final List<SourceExpr> exprs = new LinkedList<>();
				// Optional parentheses around formal parameter list
				flattenCommas(stripParens(args), args.getOffsetInParent(), exprs);
				return functionLiteral(sourceExpr, sourceOffset, exprs,
						guarantee, guaranteeSourceOffset,
						body, bodySourceOffset);
			}
		}));

	}

	private Problematic<CoreExpr> functionLiteral(SourceExpr sourceExpr, final int sourceOffset,
			List<SourceExpr> exprs,
			CoreExpr guarantee, final int guaranteeSourceOffset,
			final CoreExpr body, final int bodySourceOffset) {
		final List<FunArg> args = exprs.isEmpty() ? nonNull(Collections.<FunArg>emptyList()) : new ArrayList<FunArg>(exprs.size());
		final LinkedList<Problem> problems = new LinkedList<>();
		for(final SourceExpr argExpr : exprs) {
			final int argSourceOffset = sourceOffset + argExpr.getOffsetInParent();
			argExpr.acceptVisitor(new BaseSourceExprVisitor<Void>() {

				@Override
				@Nullable
				public Void binaryOp(final BinaryOp argOp) {
					if(argOp.getOperator() == Operator.PAIR) {
						final int nameSourceOffset = argSourceOffset + argOp.getLeft().getOffsetInParent();
						final int assertionSourceOffset = argSourceOffset + argOp.getRight().getOffsetInParent();
						final Key name = expectIdentifier(argOp.getLeft(), nameSourceOffset, problems);
						final CoreExpr assertion = expr(argOp.getRight(), assertionSourceOffset).dumpProblems(problems);
						args.add(new FunArg(argExpr.getSourceLength(), name, off(assertionSourceOffset - sourceOffset, assertion)));
						return null;
					} else {
						return fallback(argOp);
					}
				}

				@Override
				@Nullable
				public Void fallback(SourceExpr other) {
					final Key name = expectIdentifier(other, argSourceOffset, problems);
					args.add(new FunArg(argExpr.getSourceLength(), name));
					return null;
				}
			});
		}
		return result(new FunctionLiteral(sourceExpr,
				args,
				off(guaranteeSourceOffset - sourceOffset, guarantee),
				off(bodySourceOffset - sourceOffset, body)), problems);
	}

	private <L extends List<SourceExpr>> L flattenCommas(SourceExpr arg, final int offsetAdjust, final L exprs) {
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
					flattenCommas(op.getLeft(), offsetAdjust + op.getLeft().getOffsetInParent(), exprs);
					flattenCommas(op.getRight(), offsetAdjust + op.getRight().getOffsetInParent(), exprs);
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
				exprs.add(off(offsetAdjust, other));
				return null;
			}
		});
		return exprs;
	}

	class DesugarVisitor implements SourceExprVisitor<Problematic<CoreExpr>> {
		private final int sourceOffset;
		public DesugarVisitor(int sourceOffset) {
			this.sourceOffset = sourceOffset;
		}

		@Override
		public Problematic<CoreExpr> binaryOp(final BinaryOp op) {
			// Comma outside of a parentheses should be a list or map without the braces/brackets
			switch(op.getOperator()) {
			case COMMA:
			case SEMICOLON:
			case NEWLINE: {
				final LinkedList<SourceExpr> sourceExprs = flattenCommas(op, 0, new LinkedList<SourceExpr>());
				if(sourceExprs.isEmpty()) {
					final Problem problem = new ExpectedExpression(this.sourceOffset, op.getSourceLength());
					return result(new BadExpr(op, problem), problem);
				}
				return nonNull(nonNull(sourceExprs.get(0)).acceptVisitor(new BaseSourceExprVisitor<Problematic<CoreExpr>>() {
					public Problematic<CoreExpr> object() {
						return objectLiteral(op, DesugarVisitor.this.sourceOffset, sourceExprs);
					}

					public Problematic<CoreExpr> set(@Nullable Operator bullet) {
						return setLiteral(op, DesugarVisitor.this.sourceOffset, sourceExprs, bullet);
					}

					public Problematic<CoreExpr> list(@Nullable Operator bullet) {
						return listLiteral(op, DesugarVisitor.this.sourceOffset, sourceExprs, bullet);
					}

					public Problematic<CoreExpr> cond() {
						return exprListToCond(op, DesugarVisitor.this.sourceOffset, sourceExprs);
					}


					@Override
					@Nullable
					public Problematic<CoreExpr> unaryOp(UnaryOp firstOp) {
						switch(firstOp.getOperator()) {
						case TABLE_HEADER: {
							final SourceExpr second = nonNull(sourceExprs.get(1));
							return second.acceptVisitor(new BaseSourceExprVisitor<Problematic<CoreExpr>>() {
								@Override
								public banjo.parser.util.Problematic<CoreExpr> binaryOp(BinaryOp secondOp) {
									switch(secondOp.getOperator()) {
									case PAIR_INCLUDE:
									case PAIR: return object();
									default: return fallback(secondOp);
									}
								}

								@Override
								public banjo.parser.util.Problematic<CoreExpr> unaryOp(UnaryOp secondOp) {
									switch(secondOp.getOperator()) {
									case ANON_INCLUDE:
									case MIRROR: return object();
									case SET_ELEMENT: return set(secondOp.getOperator());
									case LIST_ELEMENT: return list(secondOp.getOperator());
									default: return fallback(secondOp);
									}
								}

								@Override
								public banjo.parser.util.Problematic<CoreExpr> fallback(SourceExpr other) {
									return list(null);
								}

							});
						}
						case ANON_INCLUDE:
						case MIRROR: return object();
						case LIST_ELEMENT: return list(firstOp.getOperator());
						case SET_ELEMENT: return set(firstOp.getOperator());
						default: return fallback(firstOp);
						}
					}

					@Override
					@Nullable
					public Problematic<CoreExpr> binaryOp(BinaryOp op) {
						switch(op.getOperator()) {
						case PAIR_INCLUDE:
						case PAIR: return object();
						case COND: return cond();
						default: return fallback(op);
						}
					}

					@Override
					@Nullable
					public Problematic<CoreExpr> fallback(SourceExpr other) {
						final ArrayList<CoreExpr> coreExprs = new ArrayList<>(sourceExprs.size());
						final LinkedList<Problem> problems = new LinkedList<>();
						for(final SourceExpr e : sourceExprs) {
							if(e == null) throw new NullPointerException();
							final int exprSourceOffset = DesugarVisitor.this.sourceOffset + e.getOffsetInParent();
							final CoreExpr dsExpr = expr(e, exprSourceOffset).dumpProblems(problems);
							coreExprs.add(off(exprSourceOffset - DesugarVisitor.this.sourceOffset, dsExpr));
						}
						return result(new ExprList(op, coreExprs), problems);
					}
				}));
			}
			case CALL: return call(op, this.sourceOffset);
			case FUNCTION: return functionLiteral(op, this.sourceOffset);
			case PAIR_INCLUDE:
			case PAIR: return objectLiteral(op, this.sourceOffset, nonNull(Collections.<SourceExpr>singletonList(op)));
			case ASSIGNMENT: return let(op, this.sourceOffset);
			case PROJECTION: return projection(op, this.sourceOffset);
			case MAP_PROJECTION: return optionProjection(op, this.sourceOffset);


			case GT:
			case GE:
			case LT:
			case LE:
				return comparison(op, this.sourceOffset);


				// Normal operators are translated into a method call
			case POW:
			case MUL:
			case DIV:
			case ADD:
			case SUB:
			case INTERSECT:
			case XOR:
			case UNION:
			case LOOKUP: return binaryOpToMethodCall(op, false);

			// Short-circuit operators have a lazy right operand
			case LAZY_AND:
			case LAZY_OR:
			case COND: return binaryOpToMethodCall(op, true);

			default:
				final Problem problem = new UnsupportedBinaryOperator(op.getOperator().getOp(), this.sourceOffset, op.getSourceLength());
				return result(new BadExpr(op, problem), problem);
			}
		}

		public Problematic<CoreExpr> binaryOpToMethodCall(final BinaryOp op, boolean lazyRightOperand) {
			final int leftSourceOffset = this.sourceOffset + op.getLeft().getOffsetInParent();
			final int rightSourceOffset = this.sourceOffset + op.getRight().getOffsetInParent();
			final Problematic<CoreExpr> left = expr(op.getLeft(), leftSourceOffset);
			final Problematic<CoreExpr> right = expr(op.getRight(), rightSourceOffset);
			final CoreExpr leftWithOffset = off(op.getLeft(), left.getValue());
			final CoreExpr rightWithOffset = off(op.getRight(), right.getValue());
			final CoreExpr rightMaybeLazy = lazyRightOperand ? new FunctionLiteral(rightWithOffset) : rightWithOffset;
			return result(new Call(op, new Projection(op, leftWithOffset, new Identifier(op.getOperator().getOp())), rightMaybeLazy), left, right);
		}

		@Override
		public Problematic<CoreExpr> unaryOp(final UnaryOp op) {
			final SourceExpr operandSourceExpr = op.getOperand();
			final int operandSourceOffset = this.sourceOffset + operandSourceExpr.getOffsetInParent();
			switch(op.getOperator()) {
			case LIST_ELEMENT: return singletonListLiteral(op);
			case SET_ELEMENT: return singletonSetLiteral(op);
			case LAZY: return lazyValue(op);
			case ANON_INCLUDE:
			case MIRROR: return objectLiteral(op, this.sourceOffset, nonNull(Collections.singletonList(operandSourceExpr)));
			case LIST_LITERAL: {
				final LinkedList<SourceExpr> exprs = new LinkedList<>();
				flattenCommas(operandSourceExpr, operandSourceExpr.getOffsetInParent(), exprs);
				return listLiteral(operandSourceExpr, this.sourceOffset, exprs, null);
			}
			case OBJECT_OR_SET_LITERAL: {
				// Expecting an object or set in here.
				final Problematic<CoreExpr> operandCoreExpr = expr(operandSourceExpr, operandSourceOffset);
				return nonNull(operandCoreExpr.getValue().acceptVisitor(new BaseCoreExprVisitor<Problematic<CoreExpr>>() {
					@Override
					@Nullable
					public Problematic<CoreExpr> objectLiteral(ObjectLiteral object) {
						// Have to adjust the offsets of the fields
						final Map<String, Method> fields = new LinkedHashMap<>(object.getFields().size());
						for(final Method f : object.getFields().values()) {
							fields.put(f.getKey().getKeyString(), new Method(f.getSourceLength(), f.getOffsetInObject()+operandSourceExpr.getOffsetInParent(), f.getKey(), f.getImplementation(), f.isInclude()));
						}
						return result(new ObjectLiteral(op, fields), operandCoreExpr.getProblems());
					}

					@Override
					@Nullable
					public Problematic<CoreExpr> setLiteral(SetLiteral set) {
						// Have to adjust the offsets of the elements
						final List<CoreExpr> elements = offAll(operandSourceExpr, set.getElements());
						return result(new SetLiteral(op, elements), operandCoreExpr.getProblems());
					}

					@Override
					@Nullable
					public Problematic<CoreExpr> exprList(ExprList exprList) {
						// Have to adjust the offsets of the elements
						final List<CoreExpr> elements = offAll(operandSourceExpr, exprList.getElements());
						return result(new SetLiteral(op, elements), operandCoreExpr.getProblems());
					}
					@Override
					@Nullable
					public Problematic<CoreExpr> fallback(CoreExpr other) {
						// Have to adjust the offsets of the elements
						final List<CoreExpr> elements = nonNull(Collections.singletonList(off(operandSourceExpr, other)));
						return result(new SetLiteral(op, elements), operandCoreExpr.getProblems());
					}
				}));
			}
			case PARENS:
			case RETURN:
			case UNARY_NEWLINE_INDENT:
				return expr(operandSourceExpr, operandSourceOffset);

			case OPTIONAL:
			case EXISTS:
			case NOT:
			case COMPLEMENT:
			case PLUS:
			case NEGATE:
				final String methodName = op.getOperator().getOp();
				final Problematic<CoreExpr> operandCoreExpr = expr(operandSourceExpr, operandSourceOffset);
				return result(new Call(op, new Projection(op, off(operandSourceExpr, operandCoreExpr), new Identifier(methodName))), operandCoreExpr.getProblems());

			default:
				final Problem problem = new UnsupportedUnaryOperator(op.getOperator().getOp(), this.sourceOffset, op.getSourceLength());
				return result(new BadExpr(op, problem), problem);

			}
		}

		public Problematic<CoreExpr> lazyValue(UnaryOp op) {
			final SourceExpr operandSourceExpr = op.getOperand();
			final int operandSourceOffset = this.sourceOffset + operandSourceExpr.getOffsetInParent();
			final Problematic<CoreExpr> operandCoreExpr = expr(operandSourceExpr, operandSourceOffset);
			return result(new FunctionLiteral(op.getSourceLength(), nonNull(Collections.<FunArg>emptyList()), FunctionLiteral.DEFAULT_GUARANTEE, off(operandSourceExpr, operandCoreExpr)), operandCoreExpr.getProblems());
		}

		public Problematic<CoreExpr> singletonSetLiteral(UnaryOp op) {
			final SourceExpr operandSourceExpr = op.getOperand();
			final int operandSourceOffset = this.sourceOffset + operandSourceExpr.getOffsetInParent();
			final Problematic<CoreExpr> operandCoreExpr = expr(operandSourceExpr, operandSourceOffset);
			return result(new SetLiteral(op, nonNull(Collections.singletonList(off(operandSourceExpr, operandCoreExpr)))), operandCoreExpr.getProblems());
		}

		public Problematic<CoreExpr> singletonListLiteral(UnaryOp op) {
			final SourceExpr operandSourceExpr = op.getOperand();
			final int operandSourceOffset = this.sourceOffset + operandSourceExpr.getOffsetInParent();
			final Problematic<CoreExpr> operandCoreExpr = expr(operandSourceExpr, operandSourceOffset);
			return result(new ListLiteral(op, nonNull(Collections.singletonList(off(operandSourceExpr, operandCoreExpr)))), operandCoreExpr.getProblems());
		}

		@Override
		public Problematic<CoreExpr> stringLiteral(StringLiteral stringLiteral) {
			return result(stringLiteral);
		}

		@Override
		public Problematic<CoreExpr> numberLiteral(NumberLiteral numberLiteral) {
			return result(numberLiteral);
		}

		@Override
		public Problematic<CoreExpr> identifier(Identifier simpleName) {
			return result(simpleName);
		}

		@Override
		public Problematic<CoreExpr> operator(OperatorRef operatorRef) {
			return result(operatorRef);
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
		public Problematic<CoreExpr> ellipsis(Ellipsis ellipsis) {
			final UnexpectedEllipsis error = new UnexpectedEllipsis(ellipsis, this.sourceOffset);
			return result(new BadExpr(ellipsis, error), error);
		}
		@Override
		@Nullable
		public Problematic<CoreExpr> badSourceExpr(BadSourceExpr badSourceExpr) {
			return result(new BadExpr(badSourceExpr, badSourceExpr.getError()), badSourceExpr.getError());
		}
		@Override
		@Nullable
		public Problematic<CoreExpr> emptyExpr(EmptyExpr emptyExpr) {
			return result(new ExprList(emptyExpr, nonNull(Collections.<CoreExpr>emptyList())));
		}
	}

	protected static Problematic<CoreExpr> result(final CoreExpr e) {
		return new Problematic<CoreExpr>(e);
	}

	protected static Problematic<CoreExpr> result(CoreExpr e, Problem ... problems) {
		return new Problematic<CoreExpr>(e, problems);
	}
	protected static Problematic<CoreExpr> result(CoreExpr e, List<Problem> problems) {
		return new Problematic<CoreExpr>(e, problems);
	}
	protected static Problematic<CoreExpr> result(CoreExpr e, Problematic<?> left, Problematic<?> right) {
		return new Problematic<CoreExpr>(e, left, right);
	}

}
