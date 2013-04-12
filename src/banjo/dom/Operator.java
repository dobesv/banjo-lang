package banjo.dom;

public interface Operator {

	public Precedence getPrecedence();
	public String getOp();
	public ParenType getParenType();
	public boolean isParen();
}
