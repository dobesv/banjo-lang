package banjo.dom;

import banjo.parser.util.FileRange;

public interface HasFileRange {
	public int getStartColumn();
	public int getStartOffset();

	
	public FileRange getFileRange();


}
