package banjo.dom;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;

public class ListLiteral extends AbstractExpr implements CoreExpr {
	
	private final List<CoreExpr> elements;
	
	public ListLiteral(FileRange fileRange, List<CoreExpr> elements) {
		super(fileRange);
		this.elements = Collections.unmodifiableList(elements);
	}

	public Collection<CoreExpr> getElements() {
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
		List<CoreExpr> newElements = ExprList.transformExprs(elements, transformer);
		if(newRange == fileRange && newElements == elements)
			return this;
		return new ListLiteral(newRange, newElements);
	}
	
	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitListLiteral(this);
	}
}
