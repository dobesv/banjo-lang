package banjo.parser.ast;


import banjo.parser.util.FileRange;

public class Let extends Expr {
	private final FileRange nameRange;
	private final String name;
	private final Expr value;
	
	public Let(FileRange nameRange, String name, Expr value) {
		super(new FileRange(nameRange, value.getFileRange()));
		this.nameRange = nameRange;
		this.name = name;
		this.value = value;
	}

	public Expr getValue() {
		return value;
	}

	public String getName() {
		return name;
	}

	public FileRange getNameRange() {
		return nameRange;
	}
	
	@Override
	public Precedence getPrecedence() {
		return Precedence.ASSIGNMENT;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		sb.append(name).append(" = ");
		value.toSource(sb, Precedence.ASSIGNMENT);
	}
}
