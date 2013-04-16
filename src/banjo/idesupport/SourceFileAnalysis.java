package banjo.idesupport;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.ListIterator;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.HasFileRange;
import banjo.dom.SourceExpr;
import banjo.parser.BanjoParser;
import banjo.parser.BanjoScanner;
import banjo.parser.errors.BanjoParseException;
import banjo.parser.util.FileRange;

public class SourceFileAnalysis {
	private ArrayList<HasFileRange> tokens = new ArrayList<>();
	private final int fileLength;
	private ArrayList<BanjoParseException> errors = new ArrayList<>();
	
	@Nullable
	private SourceExpr parseTree;
	
	public SourceFileAnalysis(String sourceCode) {
		BanjoScanner scanner = new BanjoScanner();
		BanjoParser parser = new BanjoParser();
		new BanjoScanner().scan(sourceCode, new TokenCollector(parser, tokens));
		errors.addAll(parser.getErrors());
		errors.addAll(scanner.getErrors());
		
		//this.ast = new BanjoDesugarer().desugar(parseTree);
		this.fileLength = sourceCode.length();
	}

	static class FakeTokenWithOffset implements HasFileRange {
		private final int offset;
		public FakeTokenWithOffset(int offset) {
			this.offset = offset;
		}
		@Override
		public int getStartOffset() { return offset; }
		@Override
		public int getStartColumn() { throw new UnsupportedOperationException(); }
		@Override
		public @NonNull FileRange getFileRange() {throw new UnsupportedOperationException(); }
	}
	static final Comparator<HasFileRange> offsetComparator = new Comparator<HasFileRange>() {
		@Override
		public int compare(HasFileRange o1, HasFileRange o2) {
			return Integer.compare(o1.getStartOffset(), o2.getStartOffset());
		}
	};
	
	/**
	 * Return a token stream for the given file range.  This allows one to do syntax highlighting
	 * by "visiting" the tokens.  The tokens returned by the resulting stream will cover the given
	 * area exactly.  Tokens spanning the boundaries of the range will be truncated to fit during
	 * the visiting process.
	 * 
	 * @param rangeStart Character offset to start at
	 * @param rangeEnd Character offset to end at (exclusive)
	 * @return A SourceTokenStream to visit tokens with
	 */
	public SourceTokenStream tokenStream(int rangeStart, int rangeEnd) {
		final HasFileRange posTok = new FakeTokenWithOffset(rangeStart);
		int pos = Collections.binarySearch(tokens, posTok, offsetComparator);
		if(pos < 0) pos = -pos - 1;
		// If there's a token that starts before rangeStart but includes rangeStart, return that one too
		while(pos > 0 && tokens.get(pos-1).getFileRange().getEndOffset() > rangeStart) pos--;
		//while(pos < tokens.size() && tokens.get(pos).getFileRange().getEndOffset() < rangeStart) pos++;
		final ListIterator<HasFileRange> it = tokens.listIterator(pos);
		
		return new SourceTokenStream(it, fileLength, rangeStart, rangeEnd);
	}

	/**
	 * Apply an increment edit.  The character at the specified offset & length are to be deleted, then
	 * the given text (which may be empty for a deletion) is inserted.
	 * 
	 * This will try to update the list of tokens to apply the given change incrementally, without parsing the entire file
	 * again.
	 * 
	 * @param offset Offset of the text to replace
	 * @param length Number of characters to delete as part of the edit
	 * @param text Characters to insert as part of the edit
	 */
	public void applyEdit(int offset, int length, String text) {
		// 1. Walk the parse tree and find the smallest subtree containing this change.
		// 2. Get the new source code for that subtree and re-parse it
		// 3. Update the tree with the new sub-tree and also update the list of tokens using the range that was parsed
		// 3. Update the file positions for all the tree nodes and tokens that follow that position
		// 
		
	}
}
