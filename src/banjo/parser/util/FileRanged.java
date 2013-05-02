package banjo.parser.util;

public class FileRanged<T> extends Container<T> {
	private final FileRange range;

	public FileRanged(FileRange range, T value) {
		super(value);
		this.range = range;
	}

	public FileRange getRange() {
		return this.range;
	}


}
