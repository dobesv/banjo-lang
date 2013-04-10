package banjo.parser.ast;

import banjo.parser.util.FileRange;

public interface HasFileRange {
	public int getStartColumn();
	public int getStartOffset();

	
	public FileRange getFileRange();


}
