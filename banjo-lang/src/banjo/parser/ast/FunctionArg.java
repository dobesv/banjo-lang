package banjo.parser.ast;

import banjo.parser.util.FileRange;

public class FunctionArg extends Expr {
	private final String name;
	private final Expr contract;
	public FunctionArg(FileRange range, String name, Expr contract) {
		super(range);
		this.name = name;
		this.contract = contract;
	}
	public String getName() {
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
	
}
