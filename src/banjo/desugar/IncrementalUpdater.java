package banjo.desugar;

import static banjo.parser.util.Check.nonNull;

import java.io.IOException;
import java.io.StringReader;
import java.util.List;
import java.util.ListIterator;
import java.util.NavigableMap;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.desugar.BanjoDesugarer.DesugarResult;
import banjo.dom.core.CoreExpr;
import banjo.dom.source.BaseSourceExprVisitor;
import banjo.dom.source.BinaryOp;
import banjo.dom.source.SourceExpr;
import banjo.dom.source.UnaryOp;
import banjo.dom.token.Identifier;
import banjo.dom.token.NumberLiteral;
import banjo.parser.BanjoParser;
import banjo.parser.BanjoParser.ExtSourceExpr;
import banjo.parser.BanjoScanner;
import banjo.parser.util.FilePos;
import banjo.parser.util.FileRange;
import banjo.parser.util.OffsetLength;
import banjo.parser.util.ParserReader;
import banjo.parser.util.SourceMap;
import banjo.parser.util.UnexpectedIOExceptionError;
import fj.P;
import fj.P2;

public class IncrementalUpdater {
	final BanjoParser parser = new BanjoParser();
	final BanjoScanner scanner = new BanjoScanner();

	static enum RangeCheck {
		/** The first argument range ends before the start of the second */
		ENDS_BEFORE,
		/** The first argument range starts after the end of the second */
		STARTS_AFTER,
		/** The end of the first range equals the start of the second */
		ENDS_AT_START,
		/** The end of the first range is inside the second, and it starts before the start of the second */
		CROSSES_START,
		/** The first range starts at the end of the second and ends after */
		STARTS_AT_END,
		/** The first range starts inside the second range and ends outside */
		CROSSES_END,
		/** The first range starts before the start of the second range and ends after the end of the second range */
		COVERS,
		/** The first range starts at the same place as the second, and ends inside */
		AT_HEAD_SHORTER,
		/** The first range starts at the same place as the second, and ends after */
		AT_HEAD_LONGER,
		/** The first range ends at the same place as the second, and starts inside */
		AT_TAIL_SHORTER, //
		/** The first range ends at the same place as the second, and starts before */
		AT_TAIL_LONGER, //
		EQUAL, // The two ranges are equal
		/** The first range starts after the second range and ends before; the reverse of COVERS */
		INSIDE,

		/** The second range is an empty range at the end of the first range */
		AT_TAIL_EMPTY,

		/** The second range is an empty range at the start of the first range */
		AT_HEAD_EMPTY; //
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
	 * @param damageRanges If non-null, ranges that were re-parsed will be added to this list
	 * @return A new AST with the change applied.
	 */
	public P2<ExtSourceExpr, DesugarResult<CoreExpr>> applyEdit(DesugarResult<CoreExpr> oldDesugarResult, ExtSourceExpr oldParseResult, int offset, int length, String replacement, String newSourceCode, @Nullable List<OffsetLength> damageRanges) {
		return updateAst(oldDesugarResult, oldParseResult, offset, length, replacement, newSourceCode, damageRanges);
	}

	/**
	 * Return an AST representing how things should be after applying the given edit.  This does
	 * its best to re-use as much of the original AST as possible.
	 * 
	 * The caller should ensure that the edit doesn't touch the start/end of the expression in such
	 * a way that the parent node should have been reparsed.
	 * 
	 * @param oldDesugarResult Previously parsed/computed AST
	 * @param editStartOffset Offset of the change
	 * @param editLength Number of characters to remove/replace
	 * @param replacement New text to insert
	 * @param damageRanges If non-null, ranges that were re-parsed will be added to this list
	 * @param problems List of problems; it will be updated in-place, so make a copy of the input list before calling
	 * @param newSourceCode Full new compilation unit source code
	 * @return
	 */

	public P2<ExtSourceExpr, DesugarResult<CoreExpr>> updateAst(DesugarResult<CoreExpr> oldDesugarResult, ExtSourceExpr oldParseResult,
			final int editStartOffset, final int editLength, final String replacement,
			final String newSourceCode, @Nullable final List<OffsetLength> damageRanges) {
		final SourceExpr oldRootSourceExpr = oldParseResult.getExpr();
		final FileRange oldFileRange = oldParseResult.getFileRange();
		final SourceMap oldSourceMaps = oldParseResult.getSourceMap();
		final ExtSourceExpr newParseResult = updateSourceTree(oldRootSourceExpr,
				oldFileRange, oldSourceMaps, editStartOffset, editLength,
				replacement, newSourceCode, damageRanges);
		final DesugarResult<CoreExpr> newDesugarResult = oldDesugarResult.redesugar(newParseResult.getExpr(), newParseResult.getSourceMap());

		@SuppressWarnings("null") @NonNull
		final P2<ExtSourceExpr, DesugarResult<CoreExpr>> result = P.p(newParseResult, newDesugarResult);
		return result;
	}

	static void addDamageRange(final @Nullable List<OffsetLength> damageRanges, int offset, int length) {
		if(damageRanges == null)
			return;

		int endOffset = offset + length;
		for(final ListIterator<OffsetLength> it = damageRanges.listIterator() ; it.hasNext() ; ) {
			final OffsetLength range = it.next();
			// If the existing range covers the new range, don't add it
			if(range.getOffset() <= offset && range.getEndOffset() >= endOffset) {
				// Don't need to add this, it's contained inside the existing damage region
				return;
			}

			// If we're only partly covered, extend our range and remove the old one
			if((range.getOffset() <= offset && range.getEndOffset() >= offset) ||
					(range.getOffset() <= endOffset && range.getEndOffset() >= offset)) {
				offset = Math.min(offset, range.getOffset());
				endOffset = Math.max(endOffset, range.getEndOffset());
				length = endOffset - offset;
				it.remove();
			}

			// Insert in sorted order from lowest to highest
			if(range.getOffset() > offset) {
				final OffsetLength newRange = new OffsetLength(offset, length);
				if(it.hasPrevious())
					damageRanges.add(it.previousIndex(), newRange);
				else
					damageRanges.add(0, newRange);
				return;
			}
		}
		final OffsetLength newRange = new OffsetLength(offset, length);
		damageRanges.add(newRange);

	}
	public ExtSourceExpr updateSourceTree(final SourceExpr oldRootSourceExpr,
			final FileRange oldFileRange,
			final SourceMap oldSourceMaps,
			final int editStartOffset, final int editLength,
			final String replacement, final String newSourceCode,
			final @Nullable List<OffsetLength> damageRanges) {

		final ParserReader in = ParserReader.fromString("", newSourceCode);
		final java.util.TreeMap<Integer, FilePos> positionMemory = new java.util.TreeMap<Integer, FilePos>();

		final OffsetLength editRange = new OffsetLength(editStartOffset, editLength);
		final int editLengthChange = replacement.length() - editLength;

		addDamageRange(damageRanges, editStartOffset, replacement.length());

		// Now we know which node to reparse, we can walk down the SourceExpr tree and update the parents and reparse the innermost damaged node
		try {
			return updateSourceTree(oldRootSourceExpr, oldFileRange, in, positionMemory, editRange, editLengthChange, replacement, oldSourceMaps, newSourceCode, damageRanges);
		} catch (final ReparseNeeded e) {
			try {
				in.seek(0);
				addDamageRange(damageRanges, 0, newSourceCode.length());
				return this.parser.parse(in);
			} catch (final IOException e1) {
				throw new UnexpectedIOExceptionError(e1);
			}
		}
	}

	static final class ReparseNeeded extends Error {
		private static final long serialVersionUID = 1L;
	}
	static final ReparseNeeded REPARSE_NEEDED = new ReparseNeeded();

	private ExtSourceExpr updateSourceTree(final SourceExpr sourceExpr, final FileRange oldRange,
			final ParserReader in, final java.util.TreeMap<Integer, FilePos> positionMemory,
			final OffsetLength editRange, final int editLengthDelta, final String replacement,
			final SourceMap oldSourceMap, final String newSourceCode,
			@Nullable final List<OffsetLength> damageRanges) throws ReparseNeeded {
		final RangeCheck rc = checkRange(oldRange, editRange);
		final FileRange newRange = adjustRange(oldRange, editRange, editLengthDelta, in, positionMemory);

		// If the node was deleted, reparse the parent
		if(newRange.length() == 0 && oldRange.length() > 0)
			throw REPARSE_NEEDED;

		// Check if any child is touching, but not completely containing, the edit.  If so, reparse.  Otherwise
		// process the children recursively
		return nonNull(sourceExpr.acceptVisitor(new BaseSourceExprVisitor<ExtSourceExpr>() {
			private ExtSourceExpr reparse() {
				try {
					final ParserReader temp = new ParserReader(new StringReader(newSourceCode), "", newRange.getEndOffset());
					temp.seek(newRange.getStart());
					addDamageRange(damageRanges, newRange.getStartOffset(), newRange.length());
					final ExtSourceExpr reparsedNode = IncrementalUpdater.this.parser.parse(temp);
					return reparsedNode;
				} catch (final IOException e) {
					throw new UnexpectedIOExceptionError(e);
				}
			}


			@Override
			public ExtSourceExpr binaryOp(BinaryOp binaryOp) {
				final SourceExpr oldLeft = binaryOp.getLeft();
				final SourceExpr oldRight = binaryOp.getRight();
				final FileRange oldLeftRange = oldSourceMap.getFirstWithin(oldRange, oldLeft);
				final FileRange rightBounds = new FileRange(oldLeftRange.getEnd(), oldRange.getEnd());
				final FileRange oldRightRange = oldSourceMap.getFirstWithin(rightBounds, oldRight);
				final RangeCheck leftRc = checkNodeRange(oldLeftRange);
				final RangeCheck rightRc = checkNodeRange(oldRightRange);
				//				if(!containsEdit(leftRc) && !containsEdit(rightRc) && shouldReparseParent(rc)) {
				//					throw REPARSE_NEEDED;
				//				}
				try {
					final ExtSourceExpr newLeft = updateSourceTree(oldLeft, oldLeftRange, in, positionMemory, editRange, editLengthDelta, replacement, oldSourceMap, newSourceCode, damageRanges);
					final ExtSourceExpr newRight = updateSourceTree(oldRight, oldRightRange, in, positionMemory, editRange, editLengthDelta, replacement, oldSourceMap, newSourceCode, damageRanges);
					final boolean childChanged = newLeft.getExpr() != oldLeft || newRight.getExpr() != oldRight;
					final BinaryOp newOp = childChanged ? new BinaryOp(binaryOp.getOperator(), newLeft.getExpr(), newRight.getExpr()) : binaryOp;
					final SourceMap newSourceMap = newLeft.getSourceMap().union(newRight.getSourceMap());
					return new ExtSourceExpr(newOp, newRange, newSourceMap);
				} catch(final ReparseNeeded e) {
					return reparse();
				}
			}


			public RangeCheck checkNodeRange(final FileRange nodeRange) {
				return checkRange(nodeRange, editRange);
			}

			/**
			 * True if the edit replaces an area contained entirely in the node we checked
			 * to generate the given RangeCheck.  This basically includes everything except
			 * STARTS_BEFORE and ENDS_AFTER.
			 */
			public boolean containsEdit(RangeCheck rc) {
				switch(rc) {
				case AT_HEAD_LONGER: // Edit is at the start of the node
				case AT_HEAD_EMPTY: // Edit is at the start of the node
				case AT_TAIL_EMPTY: // Edit is at the end of the node
				case AT_TAIL_LONGER: // Edit is at the end of the node
				case COVERS: // Node covers edit completely
				case EQUAL: // Node covers edit
					return true;

				case STARTS_AFTER: // No connection
				case ENDS_BEFORE: // No connection
				case INSIDE: // Edit obliterates node
				case AT_HEAD_SHORTER: // Edit deletes characters past the end of the node
				case CROSSES_END: // Edit deletes characters before the start of the node
				case CROSSES_START: // Edit deletes characters at the end of the node
				case STARTS_AT_END: // Edit deletes characters before the start of the node
				case ENDS_AT_START: // Edit deletes characters at the end of the node
				default:
					return false;

				}
			}

			private boolean shouldReparseParent(RangeCheck rc) {
				switch(rc) {
				case AT_HEAD_EMPTY:
				case AT_TAIL_EMPTY:
					return !(replacement.isEmpty() || isWhitespaceComments(replacement));
				case AT_HEAD_LONGER:
				case AT_HEAD_SHORTER:
				case AT_TAIL_LONGER:
				case AT_TAIL_SHORTER:
				case CROSSES_END:
				case CROSSES_START:
				case EQUAL:
				case INSIDE:
					return true;

					// Deleting whitespace/comments before or after a node doesn't change that node's contents
				case ENDS_AT_START:
				case STARTS_AT_END:
					// TODO What if it affects indentation, though ?
					return !isWhitespaceComments(replacement);

				case COVERS:
				case ENDS_BEFORE:
				case STARTS_AFTER:
					return false;
				}
				return true;
			}

			@Override
			public ExtSourceExpr unaryOp(UnaryOp unaryOp) {
				final SourceExpr oldOperand = unaryOp.getOperand();
				final FileRange operandRange = oldSourceMap.getFirstWithin(oldRange, oldOperand);
				final RangeCheck operandRc = checkNodeRange(operandRange);
				//				if(!containsEdit(operandRc) && shouldReparseParent(rc))
				//					throw REPARSE_NEEDED;
				try {
					final ExtSourceExpr newOperand = updateSourceTree(oldOperand, operandRange, in, positionMemory, editRange, editLengthDelta, replacement, oldSourceMap, newSourceCode, damageRanges);
					final boolean childChanged = newOperand.getExpr() != oldOperand;
					final UnaryOp newOp = childChanged ? new UnaryOp(unaryOp.getOperator(), newOperand.getExpr()) : unaryOp;
					return new ExtSourceExpr(newOp, newRange, newOperand.getSourceMap());
				} catch(final ReparseNeeded e) {
					return reparse();
				}
			}

			@Override
			@Nullable
			public ExtSourceExpr identifier(Identifier identifier) {
				if(editReplacesEnd() && (replacement.isEmpty() || isIdentifierSuffix(replacement)) // If the old range covers the start of the edit range, and the replacement is a viable suffix
						|| editReplacesStart() && isIdentifierPrefix(replacement) // If the old range covers the end of the edit range, and the replacement is a viable prefix
						|| editInMiddle() && isIdentifierPart(replacement)) // If the old range covers the entire edit, and the replacement is is a viable identifier part
					return reparse();

				return fallback(identifier);
			}
			/** The edit is in the middle of the node, and doesn't touch the start or end */
			public boolean editInMiddle() {
				return rc == RangeCheck.COVERS;
			}
			/** The edit replaces or extends the start of the node but leaves the end alone*/
			public boolean editReplacesStart() {
				switch(rc) {
				case AT_HEAD_EMPTY: // Edit is at the head, deleting zero characters (but might insert some)
				case CROSSES_END: // Node crosses the end of the edit
				case STARTS_AT_END: // Node starts at the end of the edit
					return true;
				default:
					return false;
				}

			}
			/** The edit replaces or extends the end of the node */
			public boolean editReplacesEnd() {
				switch(rc) {
				case AT_TAIL_EMPTY: // Edit area is zero characters at the end of the node
				case CROSSES_START: // Node crosses the start of the replaced area
				case ENDS_AT_START: // Node ends at the start of the replaced area
					return true;
				default:
					return false;
				}
			}

			@Override
			@Nullable
			public ExtSourceExpr numberLiteral(NumberLiteral numberLiteral) {
				if((editReplacesEnd() && (replacement.isEmpty() || isNumberSuffix(replacement)))
						|| (editReplacesStart() && isNumberPrefix(replacement)))
					return reparse();
				return super.numberLiteral(numberLiteral);
			}

			@Override
			@Nullable
			public ExtSourceExpr fallback(SourceExpr expr) {
				if(editInMiddle())
					return reparse();
				if(shouldReparseParent(rc))
					throw REPARSE_NEEDED;
				return new ExtSourceExpr(expr, newRange, SourceMap.empty());
			}
		}));
	}

	/** True if the string is all whitespace and/or comments */
	protected static boolean isWhitespaceComments(String replacement) {
		final int len = replacement.length();
		return BanjoScanner.scanWhitespaceAndComments(replacement, 0, len) == len;
	}

	/**
	 * True if appending the given string to any identifier would leave it still an identifier.
	 * 
	 * The string should be one or more identifier part characters followed by any number of
	 * whitespace characters.
	 */
	protected boolean isIdentifierSuffix(String replacement) {
		final int len = replacement.length();
		final int idChars = BanjoScanner.scanIdentifierPart(replacement, 0, len);
		if(idChars == 0)
			return false; // No identifier character
		final int ws = BanjoScanner.scanWhitespaceAndComments(replacement, idChars, len);
		return ws == len;
	}

	/**
	 * True if prepending the given string to any identifier would leave it still an identifier.
	 * 
	 * The string can be zero or more whitespace characters followed by an identifier start character
	 * and zero or more identifier body characters.
	 */
	protected boolean isIdentifierPrefix(String replacement) {
		final int len = replacement.length();
		final int ws = BanjoScanner.scanWhitespaceAndComments(replacement, 0, len);
		if(ws == len)
			return false; // All whitespace
		return BanjoScanner.scanIdentifierStart(replacement, ws, len) == len;
	}

	/**
	 * True if inserting the given string in the middle of any identifier would leave it still an identifier.
	 * 
	 * The string must be made entirely of identifier part characters.
	 */
	protected boolean isIdentifierPart(String replacement) {
		final int len = replacement.length();
		return BanjoScanner.scanIdentifierPart(replacement, 0, len) == len;
	}

	/**
	 * True if inserting the given string in the middle of any identifier would leave it still an identifier.
	 * 
	 * The string must be made entirely of identifier part characters.
	 */
	protected boolean isIdentifierStart(String replacement) {
		final int len = replacement.length();
		return BanjoScanner.scanIdentifierStart(replacement, 0, len) == len;
	}

	/**
	 * True if appending the given string to any number would leave it still a number.
	 * 
	 * The string would be zero or more digits followed by zero or more whitespace
	 * characters.
	 */
	protected boolean isNumberSuffix(String replacement) {
		final int len = replacement.length();
		for(int i=0; i < len; i++) {
			if(!Character.isDigit(replacement.codePointAt(i))) {
				for( ; i < len; i++) {
					if(!BanjoScanner.isWhitespaceChar(replacement.codePointAt(i)))
						return false;
				}
				return true;
			}
		}
		return true;
	}

	/**
	 * True if prepending the given string to any number would leave it still a number.
	 * 
	 * The string would be zero or more whitespace characters followed by zero or more digits.
	 */
	protected boolean isNumberPrefix(String replacement) {
		final int len = replacement.length();
		for(int i=0; i < len; i++) {
			if(!BanjoScanner.isWhitespaceChar(replacement.codePointAt(i))) {
				for( ; i < len; i++) {
					if(!Character.isDigit(replacement.codePointAt(i)))
						return false;
				}
				return true;
			}
		}
		return true;
	}

	protected FileRange adjustRange(FileRange exprRange, OffsetLength editRange, int editLengthDelta,
			final ParserReader in, java.util.TreeMap<Integer, FilePos> positionMemory) {
		final int startOffset = exprRange.getStartOffset();
		final int newStartOffset = editRange.getOffset() < startOffset ? startOffset + editLengthDelta : startOffset;
		final int endOffset = exprRange.getEndOffset();
		final int newEndOffset = editRange.getOffset() < endOffset || (editRange.getOffset() == endOffset && editLengthDelta > 0)? Math.max(newStartOffset, endOffset + editLengthDelta) : endOffset;
		return calcRange(in, positionMemory, newStartOffset, newEndOffset);
	}

	public FileRange calcRange(final ParserReader in,
			java.util.TreeMap<Integer, FilePos> positionMemory,
			final int startOffset, final int endOffset) {
		final FilePos newStartPos = calcFilePos(in, positionMemory, startOffset);
		final FilePos newEndPos = calcFilePos(in, positionMemory, endOffset);
		return new FileRange(newStartPos, newEndPos);
	}

	private FilePos calcFilePos(final ParserReader in,
			final java.util.TreeMap<Integer, FilePos> positionMemory,
			final int offset) {
		try {
			final NavigableMap<Integer, FilePos> rememberedSeeks1 = positionMemory.headMap(offset, true);
			if(!rememberedSeeks1.isEmpty()) in.seek(nonNull(rememberedSeeks1.lastEntry().getValue()));
			in.seek(offset);
			final FilePos pos = in.getFilePos();
			positionMemory.put(pos.getOffset(), pos);
			return pos;
		} catch (final IOException e) {
			throw new UnexpectedIOExceptionError(e);
		}
	}

	private RangeCheck checkRange(FileRange first, OffsetLength second) {
		final int firstStart = first.getStartOffset();
		final int firstEnd = first.getEndOffset();
		final int secondStart = second.getOffset();
		final int secondEnd = second.getEndOffset();

		if(firstEnd < secondStart) return RangeCheck.ENDS_BEFORE;
		if(firstStart > secondEnd) return RangeCheck.STARTS_AFTER;
		if(firstEnd == secondStart) {
			if(firstEnd == secondEnd) return RangeCheck.AT_TAIL_EMPTY;
			return RangeCheck.ENDS_AT_START;
		}
		if(firstStart == secondEnd) {
			if(firstStart == secondStart) return RangeCheck.AT_HEAD_EMPTY;
			return RangeCheck.STARTS_AT_END;
		}
		if(firstStart == secondStart) {
			if(firstEnd == secondEnd) return RangeCheck.EQUAL;
			if(firstEnd < secondEnd) return RangeCheck.AT_HEAD_SHORTER;
			if(firstEnd > secondEnd) return RangeCheck.AT_HEAD_LONGER;
		}
		if(firstEnd == secondEnd) {
			if(firstStart < secondStart) return RangeCheck.AT_TAIL_LONGER;
			if(firstStart > secondStart) return RangeCheck.AT_TAIL_SHORTER;
		}
		if(firstStart < secondStart) {
			if(firstEnd > secondEnd) return RangeCheck.COVERS;
			if(firstEnd > secondStart) return RangeCheck.CROSSES_START;
		}
		if(firstEnd > secondEnd) {
			if(firstStart < secondEnd) return RangeCheck.CROSSES_END;
		}

		return RangeCheck.INSIDE;
	}
}
