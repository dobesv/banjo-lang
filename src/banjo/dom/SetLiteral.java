package banjo.dom;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;

public class SetLiteral extends AbstractExpr implements CoreExpr {
	
	private final List<CoreExpr> elements;
	
	public SetLiteral(FileRange fileRange, List<CoreExpr> elements) {
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
		sb.append('{');
		boolean first = true;
		for(Expr elt : elements) {
			if(first) first = false;
			else sb.append(", ");
			elt.toSource(sb);
		}
		sb.append('}');
	}

	@Override
	public CoreExpr transform(ExprTransformer transformer) {
		List<CoreExpr> newElements = ExprList.transformExprs(elements, transformer);
		FileRange newRange = transformer.transform(fileRange);
		if(newElements == this.elements && newRange == this.fileRange)
			return this;
		return new SetLiteral(newRange, newElements);
	}

	@Override @Nullable 
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitSetLiteral(this);
	}
}
