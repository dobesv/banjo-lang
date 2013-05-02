package banjo.dom.core;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.ExprTransformer;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.parser.util.FileRange;

public class ListLiteral extends AbstractCoreExpr implements CoreExpr {
	
	private final List<CoreExpr> elements;
	
	public ListLiteral(SourceExpr sourceExpr, List<CoreExpr> elements) {
		super(sourceExpr);
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
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitListLiteral(this);
	}
}
