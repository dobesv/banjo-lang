package banjo.dom;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import banjo.parser.util.FileRange;

public class ListLiteral extends AbstractExpr {
	
	private final List<Expr> elements;
	
	public ListLiteral(FileRange fileRange, List<Expr> elements) {
		super(fileRange);
		this.elements = Collections.unmodifiableList(elements);
	}

	public Collection<Expr> getElements() {
		return elements;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}
	
	public void toSource(StringBuffer sb) {
		sb.append('[');
		boolean first = true;
		for(Expr elt : elements) {
			if(first) first = false;
			else sb.append(", ");
			elt.toSource(sb);
		}
		sb.append(']');
	}

	@Override
	public Expr transform(ExprTransformer transformer) {
		FileRange newRange = transformer.transform(fileRange);
		List<Expr> newElements = ExprList.transformExprs(elements, transformer);
		if(newRange == fileRange && newElements == elements)
			return this;
		return new ListLiteral(newRange, newElements);
	}
}
