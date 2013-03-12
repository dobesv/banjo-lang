package banjo.parser.ast;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import banjo.parser.util.FileRange;

public class SetLiteral extends Expr {
	
	private final List<Expr> elements;
	
	public SetLiteral(FileRange fileRange, List<Expr> elements) {
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
}
