package banjo.dom;


public interface Expr extends HasFileRange {

	public void toSource(StringBuffer sb);

	public Precedence getPrecedence();

	public void toSource(StringBuffer sb, Precedence outerPrec);

	public String toSource(Precedence prec);

	public String toSource();
	
	/**
	 * Return a new version of this node with the children
	 * passed through the given transformation.
	 *  
	 * @param transformer
	 * @return
	 */
	public Expr transform(ExprTransformer transformer);

}