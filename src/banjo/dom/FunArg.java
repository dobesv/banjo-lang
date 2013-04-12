package banjo.dom;

import banjo.parser.util.FileRange;

public class FunArg extends AbstractExpr {
	private final Key name;
	private final Expr contract;
	public FunArg(Key name, Expr contract) {
		super(contract == null ? name.getFileRange() : new FileRange(name.getFileRange(), contract.getFileRange()));
		this.name = name;
		this.contract = contract;
	}
	public FunArg(Key argName) {
		this(argName, null);
	}
	public Key getName() {
		return name;
	}
	public Expr getContract() {
		return contract;
	}
	@Override
	public void toSource(StringBuffer sb) {
		sb.append(name);
		if(contract != null) {
			sb.append(" : ");
			contract.toSource(sb, Precedence.ASSIGNMENT);
		}
	}
	
	@Override
	public Precedence getPrecedence() {
		if(contract != null)
			return Precedence.ASSIGNMENT;
		else
			return Precedence.ATOM;
	}
	@Override
	public Expr transform(ExprTransformer transformer) {
		Key newName = (Key)transformer.transform(name);
		Expr newContract = contract==null?null:transformer.transform(contract);
		if(newName == this.name && newContract == this.contract)
			return this;
		return new FunArg(newName, newContract);
	}
	
}
