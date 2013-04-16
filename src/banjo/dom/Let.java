package banjo.dom;


import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;

public class Let extends AbstractExpr implements CoreExpr {
	private final Key name;
	private final CoreExpr value;
	
	public Let(Key name, CoreExpr value) {
		super(new FileRange(name.getFileRange(), value.getFileRange()));
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
	public Expr transform(ExprTransformer transformer) {
		Key newName = transformer.transform(name);
		CoreExpr newValue = transformer.transform(value);
		if(newName == name && newValue == value)
			return this;
		return new Let(newName, newValue);
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitLet(this);
	}
}
