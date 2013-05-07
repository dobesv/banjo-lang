package banjo.parser.util;

public class AbstractCachedHashCode {

	private final int hashCode;

	public AbstractCachedHashCode(int hashCode) {
		this.hashCode = hashCode;
	}

	@Override
	public final int hashCode() {
		return this.hashCode;
	}
}
