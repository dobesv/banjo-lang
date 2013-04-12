package banjo.dom;


import banjo.parser.util.FileRange;

public class Let extends AbstractExpr {
	private final Key name;
	private final Expr value;
	
	public Let(Key name, Expr value) {
		super(new FileRange(name.getFileRange(), value.getFileRange()));
		this.name = name;
		this.value = value;
	}

	public Expr getValue() {
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
		// TODO Auto-generated method stub
		return null;
	}
}
