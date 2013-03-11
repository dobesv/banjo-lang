package banjo.parser.ast;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;

import banjo.parser.util.FileRange;
import banjo.parser.util.Token;

public class FunctionLiteral extends Expr {

	private final List<FunArg> args;
	private final Expr contract;
	private final Expr body;
	
	public FunctionLiteral(FileRange range, List<FunArg> args, Expr contract, Expr body) {
		super(range);
		this.args = Collections.unmodifiableList(args);
		this.contract = contract;
		this.body = body;
	}

	public List<FunArg> getArgs() {
		return args;
	}

	public Expr getBody() {
		return body;
	}
	
	@Override
	public Precedence getPrecedence() {
		return Precedence.FUNCTION;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		if(!args.isEmpty()) {
			sb.append('(');
			boolean first = true;
			for(FunArg arg : args) {
				if(first) first = false;
				else sb.append(", ");
				arg.toSource(sb, Precedence.COMMA);
			}
			sb.append(')');
		}
		if(contract != null) {
			sb.append(" : ");
			contract.toSource(sb, Precedence.FUNCTION);
		}
		sb.append(" -> ");
		
		body.toSource(sb, Precedence.FUNCTION);
	}

	public Expr getContract() {
		return contract;
	}
}
