package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.ExprTransformer;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.parser.util.FileRange;

public class FieldRef extends AbstractCoreExpr implements CoreExpr {

	private final CoreExpr object;
	private final Key key;

	public FieldRef(SourceExpr sourceExpr, CoreExpr object, Key key) {
		super(sourceExpr);
		this.object = object;
		this.key = key;
	}

	public CoreExpr getBase() {
		return object;
	}

	public String getFieldName() {
		return key.toSource();
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.SUFFIX;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		final boolean num = object instanceof NumberLiteral;
		if(num) sb.append('(');
		object.toSource(sb, Precedence.SUFFIX);
		if(num) sb.append(')');
		sb.append('.');
		key.toSource(sb);
	}

	public Key getKey() {
		return key;
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitFieldRef(this);
	}
}
