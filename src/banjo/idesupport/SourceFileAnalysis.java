package banjo.idesupport;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.ListIterator;

import banjo.parser.BanjoParser;
import banjo.parser.ast.HasFileRange;
import banjo.parser.util.FileRange;

public class SourceFileAnalysis {
	private ArrayList<HasFileRange> tokens;
	private final int fileLength;
	
	public SourceFileAnalysis(String sourceCode) {
		final BanjoParser banjoParser = new BanjoParser(sourceCode);
		try {
			banjoParser.parse();
		} catch (IOException e) {
			throw new Error(e); // From a StringReader?  never!
		}
		//this.ast = new BanjoDesugarer().desugar(parseTree);
		this.tokens = banjoParser.getTokens();
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
		public FileRange getFileRange() {throw new UnsupportedOperationException(); }
	}
	static final Comparator<HasFileRange> offsetComparator = new Comparator<HasFileRange>() {
		@Override
		public int compare(HasFileRange o1, HasFileRange o2) {
			return Integer.compare(o1.getStartOffset(), o2.getStartOffset());
		}
	};
	
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
}
