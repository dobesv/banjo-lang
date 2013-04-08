package banjo.parser.ast;

import java.util.Collections;
import java.util.List;

import banjo.parser.util.FileRange;

public class FunctionLiteral extends BaseExpr {

	private final List<FunArg> args;
	private final Expr contract;
	private final Expr body;
	
	public FunctionLiteral(FileRange range, List<FunArg> args, Expr contract, Expr body) {
		super(range);
		this.args = Collections.unmodifiableList(args);
		this.contract = contract;
		this.body = body;
	}
	
	/**
	 * Lazy expression which may have an operator in front, thus the specific text range
	 */
	public FunctionLiteral(FileRange range, Expr body) {
		this(range, Collections.<FunArg>emptyList(), null, body);
	}

	/**
	 * Lazy expression with no operator in front
	 */
	public FunctionLiteral(Expr body) {
		this(body.getFileRange(), Collections.<FunArg>emptyList(), null, body);
	}
	/**
	 * Easy unary function
	 */
	public FunctionLiteral(FunArg arg, Expr body) {
		this(new FileRange(arg.getFileRange(), body.getFileRange()), Collections.singletonList(arg), null, body);
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
		sb.append('(');
		boolean first = true;
		for(FunArg arg : args) {
			if(first) first = false;
			else sb.append(", ");
			arg.toSource(sb, Precedence.COMMA);
		}
		sb.append(')');
		
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
