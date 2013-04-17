package banjo.dom;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;

public class FieldRef extends AbstractExpr implements CoreExpr {

	private final CoreExpr object;
	private final Key key;

	public FieldRef(CoreExpr object, Key key) {
		super(new FileRange(object.getFileRange(), key.getFileRange()));
		this.object = object;
		this.key = key;
	}

	public CoreExpr getBase() {
		return object;
	}

	public String getFieldName() {
		return key.toSource();
	}

	public FileRange getFieldRange() {
		return key.getFileRange();
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
	public Expr transform(ExprTransformer transformer) {
		CoreExpr newObject = transformer.transform(object);
		Key newKey = (Key)transformer.transform(key);
		if(newObject == this.object && newKey == this.key)
			return this;
		return new FieldRef(newObject, newKey);
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitFieldRef(this);
	}
}
