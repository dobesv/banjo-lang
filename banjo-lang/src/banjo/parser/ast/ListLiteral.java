package banjo.parser.ast;

import java.util.Collection;

public class ListLiteral extends Expr {
	
	private final Collection<Expr> elements;
	
	public ListLiteral(Collection<Expr> elements) {
		this.elements = elements;
	}

	public Collection<Expr> getElements() {
		return elements;
	}

}
