package banjo.parser.errors;

import static banjo.parser.util.Check.nonNull;

import java.util.Collections;
import java.util.List;

import banjo.parser.util.FileRange;
import banjo.parser.util.OffsetLength;

public class Problem extends java.text.ParseException {
	private static final long serialVersionUID = 1L;
	@SuppressWarnings("null")
	public static final List<Problem> NONE = Collections.emptyList();
	private final int sourceLength;

	public Problem(String message, FileRange range) {
		this(message, range.getStartOffset(), range.length());
	}

	public Problem(String message, int sourceOffset, int sourceLength) {
		super(message, sourceOffset);
		this.sourceLength = sourceLength;
	}

	public Problem(String message, OffsetLength sourceOffsetLength) {
		super(message, sourceOffsetLength.getOffset());
		this.sourceLength = sourceOffsetLength.getLength();
	}

	public int getSourceOffset() {
		return getErrorOffset();
	}
	public int getSourceLength() {
		return this.sourceLength;
	}
	public int getEndOffset() {
		return getSourceOffset() + getSourceLength();
	}

	public boolean isWarning() {
		return false;
	}
	public boolean isInternalCompilerError() {
		return false;
	}
	public static List<Problem> one(Problem problem) {
		return nonNull(Collections.singletonList(problem));
	}
}