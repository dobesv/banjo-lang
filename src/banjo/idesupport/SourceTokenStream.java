package banjo.idesupport;

import java.util.EnumSet;
import java.util.ListIterator;

import banjo.dom.Comment;
import banjo.dom.HasFileRange;
import banjo.dom.NumberLiteral;
import banjo.dom.OperatorRef;
import banjo.dom.Identifier;
import banjo.dom.StringLiteral;

public class SourceTokenStream {
	private final ListIterator<HasFileRange> tokens;
	int tokenOffset;
	int tokenLength;
	final int fileLength;
	final int rangeEnd;
	public SourceTokenStream(ListIterator<HasFileRange> tokens, int fileLength, int rangeStart, int rangeEnd) {
		super();
		this.tokens = tokens;
		this.fileLength = fileLength;
		this.tokenOffset = rangeStart;
		this.tokenLength = 0;
		this.rangeEnd = rangeEnd;
	}

	public <T> T visitNext(SourceTokenVisitor<T> v) {
		this.tokenOffset = this.tokenOffset + this.tokenLength;
		if(this.tokenOffset >= this.fileLength) {
			this.tokenLength = 0;
			return v.endOfFile(this.fileLength);
		}
		if(this.tokenOffset >= this.rangeEnd) {
			this.tokenLength = 0;
			return v.endOfRange(this.rangeEnd);
		}
		
		// No more tokens?  Assume whitespace until EOF
		if(!tokens.hasNext()) {
			this.tokenLength = this.fileLength - this.tokenOffset;
			return v.whitespace(tokenOffset, tokenLength);
		}
		HasFileRange sourceItem = tokens.next();
		
		// If there's a gap, return a whitespace token
		int start = sourceItem.getStartOffset();
		if(start > this.tokenOffset) {
			tokens.previous(); // push that one back into the queue
			this.tokenLength = start - this.tokenOffset;
			return v.whitespace(tokenOffset, tokenLength);
		}
		
		final int tokenEnd = sourceItem.getFileRange().getEndOffset();
		this.tokenLength = tokenEnd - this.tokenOffset;
		if(sourceItem instanceof OperatorRef) return v.operator(tokenOffset, tokenLength);
		if(sourceItem instanceof Identifier) return v.identifier(tokenOffset, tokenLength, EnumSet.noneOf(IdentifierFlag.class));
		if(sourceItem instanceof Comment) return v.comment(tokenOffset, tokenLength);
		if(sourceItem instanceof StringLiteral) return v.stringLiteral(tokenOffset, tokenLength);
		if(sourceItem instanceof NumberLiteral) return v.numberLiteral(tokenOffset, tokenLength);
		return v.other(tokenOffset, tokenLength);
	}
}
