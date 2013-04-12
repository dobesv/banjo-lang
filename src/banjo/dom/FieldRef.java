package banjo.dom;

import banjo.parser.util.FileRange;

public class FieldRef extends AbstractExpr {

	private final Expr object;
	private final Key key;

	public FieldRef(Expr object, Key key) {
		super(new FileRange(object.getFileRange(), key.getFileRange()));
		this.object = object;
		this.key = key;
	}

	public Expr getBase() {
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
		Expr newObject = transformer.transform(object);
		Key newKey = (Key)transformer.transform(key);
		if(newObject == this.object && newKey == this.key)
			return this;
		return new FieldRef(newObject, newKey);
	}
}
