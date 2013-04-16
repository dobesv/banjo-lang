package banjo.dom;

import banjo.parser.util.FileRange;
import fj.data.Option;

public class FunArg extends AbstractExpr implements CoreExpr {
	private final Key name;
	private final Option<CoreExpr> contract;
	
	@SuppressWarnings("null")
	static final Option<CoreExpr> NO_CONTRACT = Option.none();
	
	public FunArg(Key name, Option<CoreExpr> contract) {
		super(contract.isNone() ? name.getFileRange() 
				: new FileRange(name.getFileRange(), contract.some().getFileRange()));
		this.name = name;
		this.contract = contract;
	}
	public FunArg(Key argName) {
		this(argName, NO_CONTRACT);
	}
	public Key getName() {
		return name;
	}
	public CoreExpr getContract() {
		return contract.some();
	}
	public boolean hasContract() {
		return contract.isSome();
	}
	@Override
	public void toSource(StringBuffer sb) {
		sb.append(name);
		if(contract.isSome()) {
			sb.append(": ");
			contract.some().toSource(sb, Precedence.COLON);
		}
	}
	
	@Override
	public Precedence getPrecedence() {
		if(contract.isSome())
			return Precedence.ASSIGNMENT;
		else
			return Precedence.ATOM;
	}
	@Override
	public Expr transform(ExprTransformer transformer) {
		Key newName = (Key)transformer.transform(name);
		Option<CoreExpr> newContract = optTransform(contract, transformer);
		if(newName == this.name && newContract == this.contract)
			return this;
		return new FunArg(newName, newContract);
	}
	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		throw new Error();
	}
	
}
