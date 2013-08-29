package banjo.desugar;

import static banjo.parser.util.Check.nonNull;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.Method;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.core.OffsetCoreExpr;
import banjo.dom.core.Projection;
import banjo.dom.core.SetLiteral;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.Key;
import banjo.parser.BanjoParser;
import banjo.parser.BanjoScanner;
import banjo.parser.errors.Problem;
import banjo.parser.util.OffsetLength;
import banjo.parser.util.OffsetValue;
import banjo.parser.util.ParserReader;
import banjo.parser.util.Problematic;
import banjo.parser.util.UnexpectedIOExceptionError;

public class IncrementalUpdater {
	final BanjoParser parser = new BanjoParser();
	final BanjoScanner scanner = new BanjoScanner();
	final BanjoDesugarer desugarer = new BanjoDesugarer();

	/**
	 * Update the AST, given as a node plus a list of problems and returning a new node and a new list of problems reflecting the
	 * outcome of applying that edit.
	 * 
	 * @param damageRanges If non-null, ranges that were re-parsed will be added to this list
	 */
	public <T extends CoreExpr> Problematic<T> applyEdit(Problematic<T> ast, int offset, int length, String replacement, String newSourceCode, @Nullable List<OffsetLength> damageRanges) {
		final List<Problem> problems = new ArrayList<>(ast.getProblems());
		return new Problematic<>(applyEdit(ast.getValue(), offset, length, replacement, newSourceCode, problems, damageRanges), problems);
	}

	/**
	 * Update the AST, applying the given edit.  This does its best to re-use as much of the existing
	 * AST as possible, otherwise it re-parses the source.
	 * 
	 * @param ast Original AST
	 * @param offset Offset of the edit
	 * @param length Number of bytes removed at that offset
	 * @param replacement Actual bytes inserted at that offset
	 * @param newSourceCode Complete new source code after the application of the edit
	 * @param problems List of problems; if will be updated in-place, so make a copy of the list before calling
	 * @param damageRanges If non-null, ranges that were re-parsed will be added to this list
	 * @return A new AST with the change applied.
	 */
	public <T extends CoreExpr> T applyEdit(T ast, int offset, int length, String replacement, String newSourceCode, List<Problem> problems, @Nullable List<OffsetLength> damageRanges) {
		return applyEdit(ast, ast.getOffsetInParent(), offset, length, replacement, newSourceCode, problems, damageRanges);
	}

	/**
	 * Return an AST representing how things should be after applying the given edit.  This does
	 * its best to re-use as much of the original AST as possible.
	 * 
	 * The caller should ensure that the edit doesn't touch the start/end of the expression in such
	 * a way that the parent node should have been reparsed.
	 * 
	 * @param expr Original AST
	 * @param editStartOffset Offset of the change
	 * @param editLength Number of characters to remove/replace
	 * @param replacement New text to insert
	 * @param problems List of problems; it will be updated in-place, so make a copy of the input list before calling
	 * @param damageRanges If non-null, ranges that were re-parsed will be added to this list
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public <T extends CoreExpr> T applyEdit(T expr, final int exprStartOffset, final int editStartOffset, final int editLength, final String replacement, final String newSourceCode, final List<Problem> problems, @Nullable final List<OffsetLength> damageRanges) {
		/*
		 * Different cases while transforming:
		 * 
		 * 1. Expr touching the change, but with a single child expr that touches the change,
		 *    and that child completely covers the change: transform that child, adjust range
		 * 2. Expr containing the change, but with multiple child exprs touching the change, or
		 *    the change extends beyond the edge of a single child: re-parse from source range
		 * 3. String literals, numbers, and identifiers where the change can applied and the token is still
		 *    valid: apply change
		 */

		/*
		 * Adjusting the parse tree is not a big deal.  But adjusted the desugared nodes is trickier.  The
		 * root node will always have to updated since at least one child will be updated.  So some smarts
		 * is needed to walk down the tree and do minimal damage
		 */
		// SourceExpr newContentType = parser.parse(replacement);
		final int editEndOffset = editStartOffset + editLength;
		final int exprEndOffset = exprStartOffset + expr.getSourceLength();

		// If the edit takes place entirely after or before this expression, there's nothing to do
		if(editOutside(exprStartOffset, exprEndOffset, editStartOffset, editEndOffset))
			return expr;

		final int lengthAdjust = replacement.length() - editLength;
		final int newExprSourceOffset = adjustOffset(exprStartOffset, editStartOffset, lengthAdjust, false);
		final int newExprEndOffset = adjustOffset(exprEndOffset, editStartOffset, lengthAdjust, true);

		return (T) off(expr.getOffsetInParent(), nonNull(expr.acceptVisitor(new BaseCoreExprVisitor<CoreExpr>() {

			@Override
			@Nullable
			public CoreExpr functionLiteral(FunctionLiteral n) {
				// If only the body was changed inside, then descend into the body.  Otherwise, reparse for now
				// TODO Also incrementally update FunArgs, as with object literal
				final CoreExpr body = n.getBody();
				final int bodyStartOffset = exprStartOffset + body.getOffsetInParent();
				final int bodyEndOffset = bodyStartOffset + body.getSourceLength();
				if(editInside(bodyStartOffset, bodyEndOffset)) {
					final CoreExpr newBody = process(body, bodyStartOffset);
					final int lengthDelta = newBody.getSourceLength() - body.getSourceLength();
					return new FunctionLiteral(n.getSourceLength() + lengthDelta, n.getArgs(), n.getGuarantee(), newBody);
				}
				return fallback(n);
			}

			@Override
			@Nullable
			public CoreExpr projection(Projection n) {
				final CoreExpr base = n.getObject();
				final int baseStartOffset = exprStartOffset + base.getOffsetInParent();
				final int baseEndOffset = baseStartOffset + base.getSourceLength();
				boolean contained = false;
				CoreExpr newBase;
				int baseLengthAdjust;
				if(editInside(baseStartOffset, baseEndOffset)) {
					contained = true;
					newBase = process(base, baseStartOffset);
					baseLengthAdjust = newBase.getSourceLength() - base.getSourceLength();
				} else {
					newBase = base;
					baseLengthAdjust = 0;
				}
				final CoreExpr body = n.getBody();
				final int bodyStartOffset = exprStartOffset + body.getOffsetInParent();
				final int bodyEndOffset = bodyStartOffset + body.getSourceLength();
				CoreExpr newBody;
				int bodyLengthAdjust;
				if(editInside(bodyStartOffset, bodyEndOffset)) {
					contained = true;
					newBody = process(body, bodyStartOffset);
					bodyLengthAdjust = newBody.getSourceLength() - body.getSourceLength();
				} else {
					newBody = body;
					bodyLengthAdjust = 0;
				}

				if(contained) {
					return new Projection(n.getSourceLength()+baseLengthAdjust+bodyLengthAdjust, newBase, off(baseLengthAdjust, newBody));
				}

				return fallback(n);
			}

			@Override
			@Nullable
			public CoreExpr call(Call n) {
				// For a call,
				final OffsetValue<List<CoreExpr>> newArgs = tryChildElements(n.getArguments());
				if(newArgs == null) {
					// Null means "must reparse"
					return fallback(n);
				}
				// Different list means "edit was contained in a child"
				boolean contained = newArgs.getValue() != n.getArguments();

				final CoreExpr callee = n.getObject();
				final int calleeStartOffset = exprStartOffset + callee.getOffsetInParent();
				final int calleeEndOffset = calleeStartOffset + callee.getSourceLength();
				final CoreExpr newCallee;
				final int calleeLengthDelta;
				if(editInside(calleeStartOffset, calleeEndOffset)) {
					contained = true;
					newCallee = process(callee, calleeStartOffset);
				} else {
					newCallee = adjustOffset(callee);
				}
				if(contained) {
					return new Call(n.getSourceLength() + argsLengthDelta + calleeLengthDelta, newCallee, newArgs.getValue());
				}
				return fallback(n);
			}

			@Override
			@Nullable
			public CoreExpr exprPair(ExprPair n) {
				boolean contained = false;
				final CoreExpr action = n.getAction();
				final int actionStartOffset = exprStartOffset + action.getOffsetInParent();
				final int actionEndOffset = actionStartOffset + action.getSourceLength();
				CoreExpr newAction;
				if(editInside(actionStartOffset, actionEndOffset)) {
					contained = true;
					newAction = process(action, actionStartOffset);
				} else {
					newAction = adjustOffset(action);
				}
				final CoreExpr result = n.getResult();
				final int resultStartOffset = exprStartOffset + result.getOffsetInParent();
				final int resultEndOffset = resultStartOffset + result.getSourceLength();
				CoreExpr newResult;
				if(editInside(resultStartOffset, resultEndOffset)) {
					contained = true;
					newResult = process(result, resultStartOffset);
				} else {
					newResult = adjustOffset(result);
				}
				if(contained) {
					return new ExprPair(n.getSourceLength() + actionLengthDelta + resultLengthDelta, newAction, off(resultLengthDelta, newResult));
				}
				return fallback(n);
			}

			@Override
			@Nullable
			public CoreExpr setLiteral(SetLiteral n) {
				final OffsetValue<List<CoreExpr>> newElements = tryChildElements(n.getElements());
				if(newElements != null && newElements.getValue() != n.getElements())
					return new SetLiteral(n.getSourceLength() + newElements.getOffset(), newElements.getValue());
				return fallback(n);
			}

			@Override
			@Nullable
			public CoreExpr listLiteral(ListLiteral n) {
				final OffsetValue<List<CoreExpr>> newElements = tryChildElements(n.getElements());
				if(newElements != null && newElements.getValue() != n.getElements()) {
					return new SetLiteral(n.getSourceLength() + newElements.getOffset(), newElements.getValue());
				}
				return fallback(n);
			}

			@Override
			@Nullable
			public CoreExpr objectLiteral(ObjectLiteral n) {
				boolean containedInChild = false;
				final Map<String, Method> fields = n.getMethods();
				for(final Method field : fields.values()) {
					final int fieldStartOffset = exprStartOffset + field.getOffsetInObject();
					final int fieldEndOffset = fieldStartOffset + field.getSourceLength();
					if(editOutside(fieldStartOffset, fieldEndOffset, editStartOffset, editEndOffset))
						continue;

					final Key key = field.getKey();
					final int keyStartOffset = fieldStartOffset + key.getOffsetInParent();
					final int keyEndOffset = keyStartOffset + key.getSourceLength();
					final CoreExpr value = field.getImplementation();
					final int valueStartOffset = fieldStartOffset + value.getOffsetInParent();
					final int valueEndOffset = valueStartOffset + value.getSourceLength();


					// If the edit overlaps the start/end of any part of the field, reparse
					if(editTouchesStartOrEnd(keyStartOffset, keyEndOffset) ||
							editTouchesStartOrEnd(valueStartOffset, valueEndOffset) ||
							editTouchesStartOrEnd(fieldStartOffset, fieldEndOffset)) {
						// Must reparse
						return fallback(n);
					}

					// If the edit is contined completely inside the key or value, we might avoid a reparse
					if(editInside(keyStartOffset, keyEndOffset) ||
							editInside(valueStartOffset, valueEndOffset)) {
						containedInChild = true;
						// ... don't stop looping, we might find another overlapping child for whatever reason
					}
				}

				// If the change is not contained by any child, it must be outside the child, maybe between children or on the
				// edge of the parent node, so we have to reparse the whole node.
				if(!containedInChild)
					return fallback(n);
				final LinkedHashMap<String,Method> newFields = new LinkedHashMap<>(fields);
				for(final Method field : fields.values()) {
					final int fieldStartOffset = exprStartOffset + field.getOffsetInObject();
					final Key key = field.getKey();
					final int keyStartOffset = fieldStartOffset + key.getOffsetInParent();
					final Key newKey = process(key, keyStartOffset);
					final CoreExpr value = field.getImplementation();
					final int valueStartOffset = fieldStartOffset + value.getOffsetInParent();
					final CoreExpr newValue = process(value, valueStartOffset);
					final int newSourceLength = field.getSourceLength() + lengthDelta;
					final int newOffsetInObject = field.getOffsetInObject() + offsetDelta;
					if(newKey != key || newValue != value || lengthDelta != 0 || offsetDelta != 0) {
						final Method newField = new Method(newSourceLength, newOffsetInObject, newKey, off(keyLengthDelta, newValue), field.isInclude());
						newFields.put(newKey.getKeyString(), newField);
					}
				}
				final int newObjectSourceLength = n.getSourceLength() + offsetDelta;
				return new ObjectLiteral(newObjectSourceLength, newFields);
			}

			private boolean editInside(int startOffset, int endOffset) {
				return IncrementalUpdater.editInside(startOffset, endOffset, editStartOffset, editEndOffset);
			}

			private boolean editTouchesStartOrEnd(int startOffset, int endOffset) {
				return IncrementalUpdater.editTouchesStartOrEnd(startOffset, endOffset, editStartOffset, editEndOffset);
			}

			private <TT extends CoreExpr> TT process(final TT expr, final int exprStartOffset) {
				return applyEdit(expr, exprStartOffset, editStartOffset, editLength, replacement, newSourceCode, problems, damageRanges);
			}

			/**
			 * Three types of return:
			 * 
			 * 1. null - definitely must reparse parent
			 * 2. OffsetValue(0, elements) - i.e. the same list unchanged; no child contained the change, so if there are
			 *    other child expressions (like with a call) try them, otherwise reparse.
			 * 3. OffsetValue(n, newElements) - a new elements with a length adjustment; a child was found that covers the change,
			 *    and the edit was applied to it.
			 */
			private @Nullable OffsetValue<List<CoreExpr>> tryChildElements(final List<CoreExpr> elements) {
				boolean containedInChild = false;
				for(final CoreExpr element : elements) {
					final int elementStartOffset = exprStartOffset + element.getOffsetInParent();
					final int elementEndOffset = elementStartOffset+element.getSourceLength();
					if(editInside(elementStartOffset, elementEndOffset)) {
						containedInChild = true;
						// ... don't stop looping, we might find another overlapping child for whatever reason
					} else if(editTouchesStartOrEnd(elementStartOffset, elementEndOffset)) {
						// Must reparse
						return null;
					}
				}

				// If the change is not contained by any child, it must be outside the child, maybe between children or on the
				// edge of the parent node, so we have to reparse the whole node.
				if(!containedInChild)
					return new OffsetValue<>(0, elements);

					final List<CoreExpr> newElements = new ArrayList<>(elements.size());
					int offsetDelta = 0;
					for(final CoreExpr element : elements) {
						final int elementStartOffset = exprStartOffset + element.getOffsetInParent();
						final CoreExpr newElement = process(off(offsetDelta, element), elementStartOffset);
						newElements.add(newElement);
						final int lengthDelta = newElement.getSourceLength() - element.getSourceLength();
						offsetDelta += lengthDelta;
					}
					return new OffsetValue<>(offsetDelta, newElements);
			}

			@Override
			@Nullable
			public CoreExpr fallback(CoreExpr unsupported) {
				return reparse(newSourceCode, newExprSourceOffset, newExprEndOffset, problems, damageRanges);
			}
		})));
	}

	static CoreExpr off(int offset, CoreExpr expr) {
		if(offset == 0) return expr;
		return new OffsetCoreExpr(offset, expr);
	}
	protected CoreExpr reparse(String newSourceCode, int startOffset, int endOffset, Collection<Problem> problems, @Nullable Collection<OffsetLength> damageRanges) {
		try {
			if(damageRanges != null)
				damageRanges.add(new OffsetLength(startOffset, endOffset-startOffset));

			final SourceExpr parseTree = this.parser.parse(ParserReader.fromSubstring("", newSourceCode, startOffset, endOffset)).dumpProblems(problems);
			return this.desugarer.desugar(parseTree).dumpProblems(problems);
		} catch (final IOException e) {
			throw new UnexpectedIOExceptionError(e);
		}
	}

	protected static boolean editTouchesStartOrEnd(final int exprStartOffset, final int exprEndOffset, final int editStartOffset, final int editEndOffset) {
		final boolean touchesStart = (editStartOffset <= exprStartOffset && editEndOffset >= exprStartOffset);
		final boolean touchesEnd = (editStartOffset <= exprEndOffset && editEndOffset >= exprEndOffset);
		final boolean touchesStartOrEnd = touchesStart || touchesEnd;
		return touchesStartOrEnd;
	}

	/**
	 * Adjust the given offset given the edit position and change in length.
	 * 
	 * @param offset Offset to adjust
	 * @param editStartOffset Start of the edited range
	 * @param lengthAdjust Different in length between the removed and inserted characters
	 * @param inclusive If true, adjust the offset when it equals editStartOffset, otherwise only adjust offset if it is greater than editStartOffset
	 * @return
	 */
	protected static int adjustOffset(final int offset, final int editStartOffset, final int lengthAdjust, boolean inclusive) {
		final boolean needsAdjustment = (inclusive ? offset >= editStartOffset : offset > editStartOffset);
		return needsAdjustment ? offset + lengthAdjust : offset;
	}


	/**
	 * True if the edit is completely inside the target range - that is, it is inside and doesn't touch or
	 * cross the start or end of the target range.
	 */
	protected static boolean editInside(int rangeStartOffset, int rangeEndOffset, final int editStartOffset, final int editEndOffset) {
		return editStartOffset > rangeStartOffset && editEndOffset < rangeEndOffset;
	}

	/**
	 * True if the edit is completely outside the target range - that is, it is outside and doesn't touch
	 * or cross the start or end of the target range.
	 */
	protected static boolean editOutside(int rangeStartOffset, int rangeEndOffset, final int editStartOffset, final int editEndOffset) {
		return editEndOffset < rangeStartOffset || editStartOffset > rangeEndOffset;
	}

}
