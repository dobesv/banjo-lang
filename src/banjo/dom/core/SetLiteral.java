package banjo.dom.core;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;

public class SetLiteral extends AbstractCoreExpr implements CoreExpr {

	private final List<CoreExpr> elements;

	public SetLiteral(SourceExpr sourceExpr, List<CoreExpr> elements) {
		super(sourceExpr);
		this.elements = Collections.unmodifiableList(elements);
	}

	public Collection<CoreExpr> getElements() {
		return this.elements;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append('{');
		boolean first = true;
		for(final Expr elt : this.elements) {
			if(first) first = false;
			else sb.append(", ");
			elt.toSource(sb);
		}
		sb.append('}');
	}

	@Override @Nullable
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitSetLiteral(this);
	}

	public boolean isEmpty() {
		return this.elements.isEmpty();
	}
}
