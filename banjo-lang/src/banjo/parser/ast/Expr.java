package banjo.parser.ast;


import banjo.parser.util.FileRange;

public abstract class Expr {
	private final FileRange fileRange;

	public Expr(FileRange range) {
		super();
		this.fileRange = range;
	}
	
	public int getStartColumn() { return getFileRange().getStart().getColumn(); }

	public FileRange getFileRange() {
		return fileRange;
	}
}
