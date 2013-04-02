package banjo.parser.ast;

import java.util.Collections;
import java.util.List;

import banjo.parser.util.FileRange;

public class Call extends BaseExpr {

	private final Expr callee;
	private final List<Expr> arguments;

	public Call(FileRange range, Expr callee, List<Expr> arguments) {
		super(range);
		this.callee = callee;
		this.arguments = arguments;
	}

	public Call(Expr callee, Expr arg) {
		this(new FileRange(callee.getFileRange(), arg.getFileRange()),
			 callee, 
			 Collections.singletonList(arg));
	}
	public Call(Expr callee) {
		this(callee.getFileRange(), callee, Collections.<Expr>emptyList());
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
