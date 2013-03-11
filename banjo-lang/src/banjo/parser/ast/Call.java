package banjo.parser.ast;

import java.util.List;

import banjo.parser.util.FileRange;

public class Call extends Expr {

	private final Expr callee;
	private final List<Expr> arguments;

	public Call(FileRange range, Expr callee, List<Expr> arguments) {
		super(range);
		this.callee = callee;
		this.arguments = arguments;
	}

	public Expr getCallee() {
		return callee;
	}

	public List<Expr> getArguments() {
		return arguments;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.SUFFIX;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		callee.toSource(sb, Precedence.SUFFIX);
		sb.append('(');
		boolean first = true;
		for(Expr arg : arguments) {
			if(first) first = false;
			else sb.append(", ");
			arg.toSource(sb, Precedence.COMMA.nextHighest());
		}
		sb.append(')');
	}
}
