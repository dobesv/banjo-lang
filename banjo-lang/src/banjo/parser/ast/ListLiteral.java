package banjo.parser.ast;

import java.util.Collection;

import banjo.parser.util.FileRange;

public class ListLiteral extends Expr {
	
	private final Collection<Expr> elements;
	
	public ListLiteral(FileRange fileRange, Collection<Expr> elements) {
		super(fileRange);
		this.elements = elements;
	}

	public Collection<Expr> getElements() {
		return elements;
	}

}
