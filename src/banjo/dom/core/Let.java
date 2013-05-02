package banjo.dom.core;


import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.AbstractExpr;
import banjo.dom.Expr;
import banjo.dom.ExprTransformer;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.Key;
import banjo.parser.util.FileRange;

public class Let extends AbstractCoreExpr implements CoreExpr {
	private final Key name;
	private final CoreExpr value;
	
	public Let(SourceExpr sourceExpr, Key name, CoreExpr value) {
		super(sourceExpr);
		this.name = name;
		this.value = value;
	}

	public CoreExpr getValue() {
		return value;
	}

	public Key getName() {
		return name;
	}
	
	@Override
	public Precedence getPrecedence() {
		return Precedence.ASSIGNMENT;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		name.toSource(sb);
		sb.append(" = ");
		value.toSource(sb, Precedence.ASSIGNMENT);
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitLet(this);
	}
}
