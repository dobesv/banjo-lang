package banjo.eval;

public interface CallResult {
	public <T> T acceptVisitor(CallResultVisitor<T> visitor);
}
