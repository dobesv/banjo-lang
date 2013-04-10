package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class AbstractHasFileRange implements HasFileRange {

	protected final FileRange fileRange;

	public AbstractHasFileRange(FileRange range) {
		super();
		if(range == null) throw new NullPointerException();
		this.fileRange = range;
	}

	public int getStartColumn() { return getFileRange().getStartColumn(); }

	public int getStartOffset() { return getFileRange().getStartOffset(); }

	@Override
	public FileRange getFileRange() {
		return fileRange;
	}

}