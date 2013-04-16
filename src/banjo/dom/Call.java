package banjo.dom;

import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;

public class Call extends AbstractExpr implements CoreExpr {

	private final CoreExpr callee;
	private final List<CoreExpr> arguments;

	public Call(FileRange range, CoreExpr callee, List<CoreExpr> arguments) {
		super(range);
		this.callee = callee;
		this.arguments = arguments;
	}
	
	public Call(CoreExpr callee, CoreExpr arg) {
		this(new FileRange(callee.getFileRange(), arg.getFileRange()),
			 callee, 
			 Collections.singletonList(arg));
	}
	public Call(CoreExpr callee) {
		this(callee.getFileRange(), callee, Collections.<CoreExpr>emptyList());
	}

	public CoreExpr getCallee() {
		return callee;
	}

	public List<CoreExpr> getArguments() {
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
	
	@Override
	public Expr transform(ExprTransformer transformer) {
		FileRange newRange = transformer.transform(fileRange);
		CoreExpr newCallee = transformer.transform(callee);		
		List<CoreExpr> newArgs = ExprList.transformExprs(arguments, transformer);
		if(newCallee == this.callee && 
		   newArgs == this.arguments &&
		   newRange == this.fileRange)
			return this;
		return new Call(newRange, newCallee, arguments);
	}
	
	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitCall(this);
	}
}
