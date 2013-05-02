package banjo.desugar;

import static banjo.parser.util.Check.nonNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.desugar.errors.ElseClauseNotLast;
import banjo.desugar.errors.ExpectedField;
import banjo.desugar.errors.ExpectedFieldName;
import banjo.desugar.errors.ExtraTableColumn;
import banjo.desugar.errors.InvalidProjection;
import banjo.desugar.errors.MissingElseClauseInConditional;
import banjo.desugar.errors.MissingValueForTableColumn;
import banjo.desugar.errors.MixedSemicolonAndComma;
import banjo.desugar.errors.MultipleElseClausesInConditional;
import banjo.desugar.errors.UnexpectedEllipsis;
import banjo.dom.Expr;
import banjo.dom.core.BadExpr;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprVisitorWithDefault;
import banjo.dom.core.ExprList;
import banjo.dom.core.Field;
import banjo.dom.core.FieldRef;
import banjo.dom.core.FunArg;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.Let;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.core.SetLiteral;
import banjo.dom.source.BadSourceExpr;
import banjo.dom.source.BinaryOp;
import banjo.dom.source.EmptyExpr;
import banjo.dom.source.Operator;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.SourceExprVisitor;
import banjo.dom.source.UnaryOp;
import banjo.dom.token.Ellipsis;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.dom.token.UnitRef;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.errors.ExpectedElement;
import banjo.parser.errors.ExpectedExpression;
import banjo.parser.errors.ExpectedIdentifier;
import banjo.parser.errors.UnsupportedBinaryOperator;
import banjo.parser.errors.UnsupportedUnaryOperator;
import banjo.parser.util.OffsetLength;
import fj.data.Option;

public class BanjoDesugarer implements SourceExprVisitor<CoreExpr> {
	private final LinkedList<BanjoParseException> errors = new LinkedList<>();

	// Current source expression we're using to calculate offsets from
	@Nullable SourceExpr currentSourceExpr;
	// Offset of the currentSourceExpr in the source file
	int currentSourceExprOffset;

	/**
	 * Coming in we have a tree of basically just unary and binary operations and parens and atoms.
	 * Let's enrich that a bit so we can have ObjectLiteral, ListLiteral, Let, FunctionLiteral,
	 * LetFun.
	 * 
	 * @param node
	 * @return
	 */
	public CoreExpr desugar(SourceExpr node) {
		final int prevOffset = this.currentSourceExprOffset;
		final SourceExpr prevSourceExpr = this.currentSourceExpr;
		this.currentSourceExpr = node;
		this.currentSourceExprOffset = sourceOffset(node);
		final CoreExpr result = node.acceptVisitor(this);
		if(result == null) throw new NullPointerException();
		this.currentSourceExprOffset = prevOffset;
		this.currentSourceExpr = prevSourceExpr;
		return result;
	}
	public CoreExpr desugar(SourceExpr node, Collection<BanjoParseException> errors) {
		final CoreExpr result = desugar(node);
		if(result.getSourceExpr() != node) throw new IllegalStateException("Root of desugar result must have root of parse tree as its source tree.");
		errors.addAll(this.errors);
		return result;
	}

	/**
	 * Lazy logical or operator "||".
	 * 
	 * <pre>left || right == left.if(true(t): t, false(_): right)</pre>
	 */
	private CoreExpr lazyOr(BinaryOp op) {
		//
		final CoreExpr condition = desugar(op.getLeft());
		final Field trueField = new Field(new Identifier("false"), new FunctionLiteral(desugar(op.getRight())));
		final Field falseField = new Field(new Identifier("true"), new FunctionLiteral(new Identifier("false")));
		final CoreExpr arg = new ObjectLiteral(op, trueField, falseField);
		final CoreExpr method = new FieldRef(op, condition, new Identifier("if"));
		return new Call(op, method, arg);
	}

	/**
	 * Lazy logical and operator "&&".
	 * 
	 * <pre>left && right == left.if(true(_): right, false(f): f)</pre>
	 */
	private CoreExpr lazyAnd(BinaryOp op) {
		//
		final CoreExpr condition = desugar(op.getLeft());
		final Field trueField = new Field(new Identifier("true"), new FunctionLiteral(desugar(op.getRight())));
		final Field falseField = new Field(new Identifier("false"), new FunctionLiteral(new Identifier("false")));
		final CoreExpr arg = new ObjectLiteral(op, trueField, falseField);
		final CoreExpr method = new FieldRef(op, condition, new Identifier("if"));
		return new Call(op, method, arg);
	}

	private CoreExpr comparison(BinaryOp op) {
		final CoreExpr left = desugar(op.getLeft());
		final CoreExpr right = desugar(op.getRight());
		final Call cmp = new Call(op, new FieldRef(op, left, new Identifier(nonNull(Operator.CMP.getMethodName()))), right);
		boolean checkEqual;
		String checkField;
		switch(op.getOperator()) {
		case GT: checkEqual = true; checkField = "greater"; break;
		case GE: checkEqual = false; checkField = "less"; break;
		case LT: checkEqual = true; checkField = "less"; break;
		case LE: checkEqual = false; checkField = "greater"; break;
		default: throw new Error();
		}
		final CoreExpr check = new Call(op, new FieldRef(op, cmp, new Identifier(checkField)));
		if(checkEqual)
			return check;
		else
			return new Call(op, new FieldRef(op, check, new Identifier(nonNull(Operator.NOT.getMethodName()))));
	}

	private CoreExpr projection(BinaryOp op) {
		return projection(op, op.getLeft(), op.getRight());
	}
	public int sourceOffset(final SourceExpr childExpr) {
		if(childExpr == this.currentSourceExpr) return this.currentSourceExprOffset;
		if(this.currentSourceExpr == null) return 0; // Must be the root node
		final Option<Integer> offsetFromParent = nonNull(this.currentSourceExpr).offsetToChild(childExpr);
		if(offsetFromParent.isNone()) throw new IllegalStateException("Looking for offset to node that isn't a child of the current node");
		return this.currentSourceExprOffset + offsetFromParent.orSome(0).intValue();
	}
	public OffsetLength sourceRange(final SourceExpr childExpr) {
		return new OffsetLength(sourceOffset(childExpr), childExpr.getSourceLength());
	}

	private CoreExpr projection(final SourceExpr sourceExpr,
			final SourceExpr left, final SourceExpr right) {
		final CoreExpr base = desugar(left);
		final CoreExpr projection = desugar(right);
		final CoreExpr result = projection.acceptVisitor(new CoreExprVisitorWithDefault<CoreExpr>() {
			@Override
			public CoreExpr visitStringLiteral(StringLiteral stringLiteral) {
				return new FieldRef(sourceExpr, base, stringLiteral);
			}
			@Override
			public CoreExpr visitObjectLiteral(ObjectLiteral objectLiteral) {

				return new Call(sourceExpr, new FieldRef(sourceExpr, base, new Identifier("extend")), objectLiteral);
			}

			@Override
			public CoreExpr visitSetLiteral(SetLiteral setLiteral) {
				final SetLiteral fieldSet = (SetLiteral) projection;
				final LinkedHashMap<String,Field> fields = new LinkedHashMap<>(fieldSet.getElements().size());
				for(final CoreExpr e : fieldSet.getElements()) {
					Key key;

					final SourceExpr childExpr = e.getSourceExpr();
					if(e instanceof Identifier) {
						key = (Identifier)e;
					} else if(e instanceof StringLiteral) {
						key = (StringLiteral)e;
					} else {
						getErrors().add(new ExpectedIdentifier(sourceRange(childExpr)));
						continue;
					}
					fields.put(key.getKeyString(), new Field(key, new FieldRef(childExpr, base, key)));

				}
				return new ObjectLiteral(sourceExpr, fields);
			}

			@Override
			public CoreExpr visitIdentifier(Identifier simpleName) {
				return new FieldRef(sourceExpr, base, simpleName);
			}

			@Override
			public CoreExpr fallback(CoreExpr x) {
				getErrors().add(new InvalidProjection(x.getSourceExpr().toSource(), sourceRange(x.getSourceExpr())));
				return base;
			}
		});
		if(result == null) throw new NullPointerException();
		return result;
	}


	/**
	 * Optional projection is a projection that works on an option type - it gives a
	 * new option value which is the result of the projection.  The right-hand side
	 * is only evaluated if the option value is present.
	 * 
	 * <pre> x?.foo == x.map((x) -> x.foo)</pre>
	 */
	private CoreExpr optionProjection(BinaryOp op) {
		final Identifier argName = gensym();
		final CoreExpr projection = projection(op, argName, op.getRight());
		final CoreExpr projectFunc = new FunctionLiteral(op, new FunArg(op, argName), projection);
		final CoreExpr left = desugar(op.getLeft());
		final CoreExpr mapCall = new Call(op, new FieldRef(op, left, new Identifier("map")), projectFunc);
		return mapCall;
	}

	/**
	 * The "or else" operator "?:" operates on an option value.  If the option has
	 * a value it returns that value, otherwise it returns the right-hand value.  The
	 * right-hand side is only evaluated if the option value is not present.
	 * 
	 * <pre>x ?: y == x.orElse(-> y)</pre>
	 */
	private CoreExpr orElse(BinaryOp op) {
		final CoreExpr left = desugar(op.getLeft());
		final CoreExpr right = desugar(op.getRight());
		return new Call(op, new FieldRef(op, left, new Identifier("valueOrElse")), new FunctionLiteral(right));
	}

	int gensymCounter = 0;
	private Identifier gensym() {
		this.gensymCounter++;
		return new Identifier("__t"+this.gensymCounter);
	}

	private CoreExpr call(BinaryOp op) {
		List<CoreExpr> args;
		final CoreExpr callee = desugar(op.getLeft());
		final CoreExpr arg = desugar(op.getRight());
		if(arg instanceof ExprList) {
			args = ((ExprList) arg).getElements();
		} else {
			args = Collections.singletonList(arg);
		}
		return new Call(op, callee, nonNull(args));
	}

	private CoreExpr let(BinaryOp op) {
		SourceExpr target = stripParens(op.getLeft()); // Optional parens around lhs if it has a contract spec
		final SourceExpr value = op.getRight();
		@Nullable CoreExpr contract = FunctionLiteral.CONTRACT_NONE;
		CoreExpr dsValue;
		if(isPair(target)) {
			final BinaryOp targetBOp = (BinaryOp)target;
			target = targetBOp.getLeft();
			contract = desugar(targetBOp.getRight());
		}
		if(isCallWithArgs(target)) {
			final BinaryOp call = (BinaryOp) target;
			dsValue = functionLiteral(op, call.getRight(), contract, desugar(value), null);
			target = call.getLeft();
			contract = FunctionLiteral.CONTRACT_NONE;
		} else if(isUnaryCall(target)) {
			final UnaryOp call = (UnaryOp) target;
			dsValue = new FunctionLiteral(op, nonNull(Collections.<FunArg>emptyList()), contract, desugar(value));
			target = call.getOperand();
			contract = FunctionLiteral.CONTRACT_NONE;
		} else {
			dsValue = desugar(value);
		}
		if(target instanceof Identifier) {
			final Identifier id = (Identifier) target;
			if(contract != null) {
				dsValue = new Call(op, contract, dsValue);
			}
			return new Let(op, id, dsValue);
		} else {
			getErrors().add(new ExpectedIdentifier(target, sourceRange(target)));
			return dsValue;
		}
	}

	private boolean isUnaryCall(SourceExpr target) {
		return isUnaryOp(target, Operator.UNARY_CALL);
	}

	private boolean isCallWithArgs(SourceExpr target) {
		return target instanceof BinaryOp && ((BinaryOp)target).getOperator() == Operator.CALL;
	}

	/**
	 * Requires Boolean to have a method <code>lazyIfTrue(trueFunc,falseFunc): ifTrue(trueFunc,falseFunc)()</code>
	 */
	private CoreExpr exprListToCond(SourceExpr sourceExpr, LinkedList<SourceExpr> exprs) {
		CoreExpr result = null;
		boolean missingElseClause = false;
		SourceExpr duplicateElseClause = null;
		for(final Iterator<SourceExpr> it = exprs.descendingIterator(); it.hasNext(); ) {
			final SourceExpr e = nonNull(it.next());
			if(isCondCase(e)) {
				final BinaryOp caseOp = (BinaryOp)e;
				final CoreExpr thenExpr = desugar(caseOp.getRight());
				if(caseOp.getLeft() instanceof Ellipsis) {
					if(result != null) {
						duplicateElseClause = e;
					}
					result = thenExpr;
				} else {
					if(result == null) {
						missingElseClause = true;
						result = new Identifier("~~~MISSING ELSE~~~");
					}

					final CoreExpr condition = desugar(caseOp.getLeft());
					final Field trueField = new Field(new Identifier("true"), new FunctionLiteral(thenExpr));
					final Field falseField = new Field(new Identifier("false"), new FunctionLiteral(result));
					final CoreExpr arg = new ObjectLiteral(e, trueField, falseField);
					final CoreExpr ifMethod = new FieldRef(e, condition, new Identifier("if"));
					result = new Call(e, ifMethod, arg);
				}
			} else {
				if(result != null) {
					duplicateElseClause = e;
				}
				result = desugar(e);
			}
		}

		if(duplicateElseClause != null) {
			if(missingElseClause) {
				// Else clause too early
				getErrors().add(new ElseClauseNotLast(sourceRange(duplicateElseClause)));
			} else {
				getErrors().add(new MultipleElseClausesInConditional(sourceRange(duplicateElseClause)));
			}
		} else if(missingElseClause) {
			getErrors().add(new MissingElseClauseInConditional(sourceRange(sourceExpr)));
		}
		// result would only be null if the
		if(result == null) throw new NullPointerException();
		return result;
	}

	private final boolean isCondCase(Expr e) {
		return (e instanceof BinaryOp) && ((BinaryOp)e).getOperator() == Operator.COND;
	}

	private CoreExpr listLiteral(SourceExpr sourceExpr, List<SourceExpr> list, @Nullable Operator requireBullet) {
		return new ListLiteral(sourceExpr, elements(list, requireBullet));
	}

	private CoreExpr setLiteral(SourceExpr sourceExpr, List<SourceExpr> list, @Nullable Operator requireBullet) {
		return new SetLiteral(sourceExpr, elements(list, requireBullet));
	}

	private ArrayList<CoreExpr> elements(List<SourceExpr> list, @Nullable Operator requireBullet) {
		final ArrayList<CoreExpr> elements = new ArrayList<>(list.size());
		List<SourceExpr> headings = null;
		if(!list.isEmpty()) {
			for(final SourceExpr e : list) {
				if(e == null) throw new NullPointerException();
				if(isTableHeader(e)) {
					final UnaryOp headerOp = (UnaryOp)e;
					headings = flattenCommasOrSemicolons(headerOp.getOperand(), new ArrayList<SourceExpr>());
					continue;
				}
				SourceExpr eltExpr;
				if(requireBullet != null && isUnaryOp(e, requireBullet)) {
					eltExpr = ((UnaryOp)e).getOperand();
				} else {
					if(requireBullet != null) {
						// TODO Wrong error
						getErrors().add(new ExpectedElement(sourceOffset(e), e.getSourceLength()));
					}
					eltExpr = e;
				}
				if(headings != null) {
					elements.add(makeRow(headings, eltExpr));
				} else {
					elements.add(desugar(eltExpr));
				}
			}
		}
		return elements;
	}

	private CoreExpr objectLiteral(SourceExpr sourceExpr, Collection<SourceExpr> pairs) {
		// Key/value pair - treat as an object
		final LinkedHashMap<String, Field> fields = new LinkedHashMap<>(pairs.size()*2);
		List<SourceExpr> headings = null;
		for(final SourceExpr e : pairs) {
			if(e == null) throw new NullPointerException();
			SourceExpr keyExpr;
			SourceExpr valueExpr;
			if(isPair(e)) {
				final BinaryOp fieldOp = (BinaryOp)e;
				keyExpr = fieldOp.getLeft();
				valueExpr = fieldOp.getRight();
			} else if(isMirrorElement(e)) {
				final UnaryOp fieldOp = (UnaryOp)e;
				keyExpr = valueExpr = fieldOp.getOperand();
			} else if(isTableHeader(e)) {
				final UnaryOp headerOp = (UnaryOp)e;
				headings = flattenCommasOrSemicolons(headerOp.getOperand(), new ArrayList<SourceExpr>());
				continue;
			} else {
				getErrors().add(new ExpectedField(sourceRange(e)));
				continue;
			}
			addField(keyExpr, valueExpr, headings, fields);
		}
		return new ObjectLiteral(sourceExpr, fields);
	}

	private void addField(SourceExpr keyExpr, SourceExpr valueExpr,
			@Nullable List<SourceExpr> headings, LinkedHashMap<String, Field> fields) {
		@Nullable CoreExpr contract = FunctionLiteral.CONTRACT_NONE;
		if(isPair(keyExpr)) {
			final BinaryOp targetBOp = (BinaryOp)keyExpr;
			keyExpr = targetBOp.getLeft();
			contract = desugar(targetBOp.getRight());
		}
		CoreExpr value;
		@Nullable Key selfName = null;
		if(isCallWithArgs(keyExpr)) {
			final BinaryOp call = (BinaryOp) keyExpr;
			keyExpr = call.getLeft();
			if(isProjection(keyExpr)) {
				final SourceExpr selfExpr = ((BinaryOp)keyExpr).getLeft();
				if(selfExpr instanceof Key) {
					selfName = (Key)selfExpr;
				} else {
					selfName = new Identifier(selfExpr.toSource());
					this.errors.add(new ExpectedIdentifier(selfExpr, sourceRange(selfExpr)));
				}
				keyExpr = ((BinaryOp)keyExpr).getRight();
			}
			value = functionLiteral(valueExpr, flattenCommasOrSemicolons(call.getRight(), new ArrayList<SourceExpr>()), desugar(valueExpr), contract, selfName);
			contract = FunctionLiteral.CONTRACT_NONE;
		} else if(isUnaryCall(keyExpr)) {
			final UnaryOp call = (UnaryOp) keyExpr;
			keyExpr = call.getOperand();
			if(isProjection(keyExpr)) {
				final SourceExpr selfExpr = ((BinaryOp)keyExpr).getLeft();
				if(selfExpr instanceof Key) {
					selfName = (Key)selfExpr;
				} else {
					selfName = new Identifier(selfExpr.toSource());
					this.errors.add(new ExpectedIdentifier(selfExpr, sourceRange(selfExpr)));
				}
				keyExpr = ((BinaryOp)keyExpr).getRight();
			}
			final List<SourceExpr> args = Collections.<SourceExpr>emptyList();
			if(args == null) throw new NullPointerException();
			value = functionLiteral(valueExpr, args, desugar(valueExpr), contract, selfName);
			contract = FunctionLiteral.CONTRACT_NONE;
		} else if(headings != null) {
			// If this is a table, construct the row
			value = makeRow(headings, valueExpr);
		} else {
			value = desugar(valueExpr);
		}
		if(contract != null) {
			// TODO Figure out how to call the contract properly ...
			final CoreExpr c = nonNull(contract);
			value = new Call(c.getSourceExpr(), c, value);
		}
		if(!(keyExpr instanceof Key)) {
			getErrors().add(new ExpectedFieldName("Expected identifier or string; got "+keyExpr.getClass().getSimpleName()+" '"+keyExpr.toSource()+"'", sourceRange(keyExpr)));
			keyExpr = new Identifier(keyExpr.toSource());
		}
		final Key key = (Key)keyExpr;
		final Field field = new Field(key, value);
		fields.put(field.getKey().getKeyString(), field);
	}

	private boolean isProjection(SourceExpr keyExpr) {
		return isBinaryOp(keyExpr, Operator.PROJECTION);
	}

	private boolean isBinaryOp(SourceExpr expr, Operator opToCheckFor) {
		return((expr instanceof BinaryOp) && ((BinaryOp) expr).getOperator() == opToCheckFor);
	}

	private CoreExpr makeRow(List<SourceExpr> headings, SourceExpr rowExpr) {
		final List<SourceExpr> values = flattenCommasOrSemicolons(stripParens(rowExpr), new ArrayList<SourceExpr>(headings.size()));
		for(int i=headings.size(); i < values.size(); i++) {
			final SourceExpr val = values.get(i);
			getErrors().add(new ExtraTableColumn(i, headings.size(), val.toSource(), sourceRange(val)));
		}
		if(values.size() < headings.size()) {
			getErrors().add(new MissingValueForTableColumn(values.size(), headings.size(), sourceRange(rowExpr), headings.get(values.size()).toSource()));
		}
		final LinkedHashMap<String, Field> fields = new LinkedHashMap<>(headings.size()*2);
		for(int i=0; i < values.size(); i++) {
			addField(nonNull(headings.get(i)), nonNull(values.get(i)), null, fields);
		}
		return new ObjectLiteral(rowExpr, fields);
	}

	/**
	 * If the expression is wrapped in parenthesis, remove them.
	 * @param valueExpr
	 * @return
	 */
	private SourceExpr stripParens(SourceExpr valueExpr) {
		if(isUnaryOp(valueExpr, Operator.PARENS))
			return ((UnaryOp)valueExpr).getOperand();
		return valueExpr;
	}

	private CoreExpr functionLiteral(BinaryOp op) {
		final SourceExpr argsDef = op.getLeft();
		final SourceExpr body = op.getRight();
		return functionLiteral(argsDef, desugar(body), op);
	}

	private CoreExpr functionLiteral(SourceExpr argsDef, final CoreExpr body,
			SourceExpr sourceExpr) {
		@Nullable CoreExpr returnContract = FunctionLiteral.CONTRACT_NONE;
		@Nullable Key selfName = null;
		// Optional self-name
		if(isBinaryOp(argsDef, Operator.PROJECTION)) {
			selfName = expectKey(((BinaryOp)argsDef).getLeft());
			argsDef = ((BinaryOp)argsDef).getRight();
		}
		// Optional return type/contract
		if(isPair(argsDef)) {
			returnContract = desugar(((BinaryOp)argsDef).getRight());
			argsDef = ((BinaryOp)argsDef).getLeft();
		}
		return functionLiteral(sourceExpr, argsDef, returnContract, body, selfName);
	}

	/**
	 * If the given parameter is a not an Identifier or StringLiteral, report an error and return
	 * a fake identifier.
	 * 
	 * @param e
	 * @return
	 */
	private Key expectKey(SourceExpr e) {
		try {
			return (Key)e;
		} catch(final ClassCastException x) {
			this.errors.add(new ExpectedIdentifier(e, sourceRange(e)));
			return new Identifier(e.toSource());
		}
	}
	private CoreExpr functionLiteral(SourceExpr op, SourceExpr args, @Nullable CoreExpr contract, final CoreExpr body, @Nullable Key selfName) {
		// Args should be comma-separated
		final List<SourceExpr> exprs = new LinkedList<>();
		if(args instanceof UnitRef) {
			// Leave the list empty if args is an empty list of some kind
		} else {
			// Optional parentheses around formal parameter list
			flattenCommas(stripParens(args), Operator.COMMA, exprs);
		}
		return functionLiteral(op, exprs, body, contract, selfName);
	}

	private CoreExpr functionLiteral(SourceExpr op, List<SourceExpr> exprs,
			final CoreExpr body, @Nullable CoreExpr returnContract, @Nullable Key selfName) {
		final List<FunArg> args = exprs.isEmpty() ? Collections.<FunArg>emptyList() : new ArrayList<FunArg>(exprs.size());
		if(args == null) throw new NullPointerException();
		for(final SourceExpr argExpr : exprs) {
			SourceExpr name = argExpr;
			if(name == null) throw new NullPointerException();
			@Nullable CoreExpr contract = FunctionLiteral.CONTRACT_NONE;
			if(isPair(name)) {
				final BinaryOp pair = (BinaryOp) name;
				name = pair.getLeft();
				contract = desugar(pair.getRight());
			}
			if(!(name instanceof Key)) {
				getErrors().add(new ExpectedIdentifier(name, sourceRange(name)));
				continue;
			}
			args.add(new FunArg(op, (Key)name, contract));
		}
		return new FunctionLiteral(op, selfName, args, returnContract, body);
	}

	private boolean isListElement(SourceExpr e) {
		return isUnaryOp(e, Operator.LIST_ELEMENT);
	}
	private boolean isSetElement(SourceExpr e) {
		return isUnaryOp(e, Operator.SET_ELEMENT);
	}

	private boolean isPair(SourceExpr e) {
		return (e instanceof BinaryOp) &&
				(((BinaryOp) e).getOperator() == Operator.PAIR);
	}
	private boolean isMirrorElement(SourceExpr e) {
		return isUnaryOp(e, Operator.MIRROR);
	}
	private boolean isTableHeader(SourceExpr e) {
		return isUnaryOp(e, Operator.TABLE_HEADER);
	}

	private boolean isUnaryOp(SourceExpr e, final Operator operator) {
		return (e instanceof UnaryOp) && ((UnaryOp) e).getOperator() == operator;
	}

	private List<SourceExpr> flattenCommas(SourceExpr arg, Operator type, List<SourceExpr> exprs) {
		if(arg instanceof BinaryOp) {
			final BinaryOp bop = (BinaryOp) arg;
			if(isElementSeparator(bop)) {
				final Operator boper = bop.getOperator();
				if(boper != type && bop.getOperator() != Operator.NEWLINE) {
					if(type == Operator.NEWLINE) type = bop.getOperator();
					else getErrors().add(new MixedSemicolonAndComma(bop, sourceRange(bop)));
				}
				flattenCommas(bop.getLeft(), type, exprs);
				flattenCommas(bop.getRight(), type, exprs);
				return exprs;
			}
		}

		if(!(arg instanceof EmptyExpr)) {
			exprs.add(arg);
		}
		return exprs;
	}

	private List<SourceExpr> flattenCommasOrSemicolons(SourceExpr arg, List<SourceExpr> list) {
		if(arg instanceof BinaryOp) {
			final BinaryOp op = (BinaryOp) arg;
			if(isElementSeparator(op)) {
				flattenCommas(op, op.getOperator(), list);
				return list;
			}
		}
		if(!(arg instanceof EmptyExpr)) {
			list.add(arg);
		}
		return list;
	}

	private static boolean isElementSeparator(BinaryOp op) {
		return isElementSeparator(op.getOperator());
	}

	private static boolean isElementSeparator(final Operator operator) {
		return operator == Operator.COMMA || operator == Operator.SEMICOLON || operator == Operator.NEWLINE;
	}

	public LinkedList<BanjoParseException> getErrors() {
		return this.errors;
	}

	@Override
	public CoreExpr visitBinaryOp(BinaryOp op) {
		// Comma outside of a parentheses should be a list or map without the braces/brackets
		switch(op.getOperator()) {
		case COMMA:
		case SEMICOLON:
		case NEWLINE: {
			final LinkedList<SourceExpr> exprs = new LinkedList<>();
			flattenCommas(op, op.getOperator(), exprs);
			final SourceExpr first = exprs.get(0);
			if(first == null) throw new NullPointerException();
			if(isTableHeader(first)) {
				final SourceExpr second = exprs.get(1);
				if(second == null) throw new NullPointerException();
				if(isPair(second) || isMirrorElement(second)) {
					return objectLiteral(op, exprs);
				} else if(isSetElement(second)){
					return setLiteral(op, exprs, ((UnaryOp)second).getOperator());
				} else if(isListElement(second)) {
					return listLiteral(op, exprs, ((UnaryOp)second).getOperator());
				} else {
					return listLiteral(op, exprs, null);
				}
			} else if(isPair(first) || isMirrorElement(first)) {
				return objectLiteral(op, exprs);
			} else if(isListElement(first)) {
				// Bulleted list item - treat as a list
				return listLiteral(op, exprs, ((UnaryOp)first).getOperator());
			} else if(isSetElement(first)) {
				// Bulleted set item - treat as a list
				return setLiteral(op, exprs, ((UnaryOp)first).getOperator());
			} else if(isCondCase(first)) {
				return exprListToCond(op, exprs);
			} else {
				// Everything else - treat as a series of steps
				final ArrayList<CoreExpr> exprList = new ArrayList<>(exprs.size());
				for(final SourceExpr e : exprs) {
					if(e == null) throw new NullPointerException();
					exprList.add(desugar(e));
				}
				return new ExprList(op, exprList);
			}
		}
		case LAZY_AND: return lazyAnd(op);
		case LAZY_OR: return lazyOr(op);
		case COND: return exprListToCond(op, new LinkedList<SourceExpr>(Collections.singletonList(op)));
		case CALL: return call(op);
		case FUNCTION: return functionLiteral(op);
		case PAIR: return objectLiteral(op, nonNull(Collections.<SourceExpr>singletonList(op)));
		case ASSIGNMENT: return let(op);
		case PROJECTION: return projection(op);
		case MAP_PROJECTION: return optionProjection(op);
		case OR_ELSE: return orElse(op);
		case GT:
		case GE:
		case LT:
		case LE:
			return comparison(op);

			// TODO Eliminate ALL binary ops as function calls
		default:
			final String methodName = op.getOperator().getMethodName();
			if(methodName != null)
				return new Call(op, new FieldRef(op, desugar(op.getLeft()), new Identifier(methodName)), desugar(op.getRight()));
			getErrors().add(new UnsupportedBinaryOperator(op.getOperator().getOp(), sourceRange(op)));
			return desugar(op.getRight());
		}
	}

	@Override
	public CoreExpr visitUnaryOp(UnaryOp op) {
		final SourceExpr operand = op.getOperand();
		switch(op.getOperator()) {
		case LIST_ELEMENT: return new ListLiteral(op, nonNull(Collections.singletonList(desugar(operand))));
		case SET_ELEMENT: return new SetLiteral(op, nonNull(Collections.singletonList(desugar(operand))));
		case LAZY: return new FunctionLiteral(op, nonNull(Collections.<FunArg>emptyList()), FunctionLiteral.CONTRACT_NONE, desugar(operand));
		case MIRROR: return objectLiteral(op, nonNull(Collections.singletonList(operand)));
		case LIST_LITERAL: {
			final LinkedList<SourceExpr> exprs = new LinkedList<>();
			flattenCommasOrSemicolons(operand, exprs);
			return listLiteral(operand, exprs, null);
		}
		case OBJECT_OR_SET_LITERAL: { // Expecting an object or set
			final CoreExpr dsOperand = desugar(operand);
			if(dsOperand instanceof ObjectLiteral) return dsOperand;
			if(dsOperand instanceof SetLiteral) return dsOperand;
			if(dsOperand instanceof ExprList) {
				final List<CoreExpr> elements = ((ExprList)dsOperand).getElements();
				return new SetLiteral(op, elements);
			}
			// Singleton set literal
			return new SetLiteral(op, nonNull(Collections.singletonList(dsOperand)));
		}
		case UNARY_CALL:
			return new Call(op, desugar(operand));
		case PARENS:
		case RETURN:
		case UNARY_NEWLINE_INDENT:
			return desugar(operand);
		default: {
			final CoreExpr dsOperand = desugar(operand);
			final String methodName = op.getOperator().getMethodName();
			if(methodName != null) {
				return new Call(op, new FieldRef(op, dsOperand, new Identifier(methodName)));
			}
			getErrors().add(new UnsupportedUnaryOperator(op.getOperator().getOp(), sourceRange(op)));
			return dsOperand;
		}
		}
	}

	@Override
	public CoreExpr visitStringLiteral(StringLiteral stringLiteral) {
		return stringLiteral;
	}

	@Override
	public CoreExpr visitNumberLiteral(NumberLiteral numberLiteral) {
		return numberLiteral;
	}

	@Override
	public CoreExpr visitIdentifier(Identifier simpleName) {
		return simpleName;
	}

	@Override
	public CoreExpr visitUnit(UnitRef unit) {
		switch(unit.getParenType()) {
		case BRACES: return new ObjectLiteral(unit, nonNull(Collections.<String,Field>emptyMap()));
		case BRACKETS: return new ListLiteral(unit, nonNull(Collections.<CoreExpr>emptyList()));
		default:
		case PARENS:
			getErrors().add(new ExpectedExpression(sourceRange(unit), unit.toSource()));
			return new Identifier(unit.toSource());
		}
	}

	@Override
	public CoreExpr visitOperator(OperatorRef operatorRef) {
		return operatorRef;
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
	public CoreExpr visitEllipsis(Ellipsis ellipsis) {
		final UnexpectedEllipsis error = new UnexpectedEllipsis(ellipsis, sourceRange(ellipsis));
		getErrors().add(error);
		return new BadExpr(ellipsis, error);
	}
	@Override
	@Nullable
	public CoreExpr visitBadSourceExpr(BadSourceExpr badSourceExpr) {
		return new BadExpr(badSourceExpr, badSourceExpr.getError());
	}
	@Override
	@Nullable
	public CoreExpr visitEmpty(EmptyExpr emptyExpr) {
		return new ExprList(emptyExpr, nonNull(Collections.<CoreExpr>emptyList()));
	}
}
