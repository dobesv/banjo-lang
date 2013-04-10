package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class Comment extends AbstractHasFileRange {
	private final String text;

	public Comment(FileRange range, String body) {
		super(range);
		this.text = body;
	}

	public String getText() {
		return text;
	}
	
}
