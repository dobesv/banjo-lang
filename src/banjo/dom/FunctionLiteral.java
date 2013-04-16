package banjo.dom;

import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.parser.util.FileRange;
import fj.data.Option;

public class FunctionLiteral extends AbstractExpr implements CoreExpr {

	private final List<FunArg> args;
	private final Option<CoreExpr> contract;
	private final CoreExpr body;
	
	public static final Option<CoreExpr> CONTRACT_NONE = Option.none();
	
	public FunctionLiteral(FileRange range, List<FunArg> args, Option<CoreExpr> contract, CoreExpr body) {
		super(range);
		this.args = Collections.unmodifiableList(args);
		this.contract = contract;
		this.body = body;
	}
	
	/**
	 * Lazy expression which may have an operator in front, thus the specific text range
	 */
	public FunctionLiteral(FileRange range, CoreExpr body) {
		this(range, Collections.<FunArg>emptyList(), CONTRACT_NONE, body);
	}

	/**
	 * Lazy expression with no operator in front
	 */
	public FunctionLiteral(CoreExpr body) {
		this(body.getFileRange(), Collections.<FunArg>emptyList(), CONTRACT_NONE, body);
	}
	/**
	 * Easy unary function
	 */
	public FunctionLiteral(FunArg arg, CoreExpr body) {
		this(new FileRange(arg.getFileRange(), body.getFileRange()), Collections.singletonList(arg), CONTRACT_NONE, body);
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
			arg.toSource(sb);
		}
		sb.append(')');
		
		if(contract.isSome()) {
			sb.append(" : ");
			contract.some().toSource(sb, Precedence.FUNCTION);
		}
		sb.append(" -> ");
		
		body.toSource(sb, Precedence.FUNCTION);
	}

	public CoreExpr getContract() {
		return contract.some();
	}
	
	public boolean hasContract() {
		return contract.isSome();
	}

	@Override
	public Expr transform(ExprTransformer transformer) {
		FileRange newRange = transformer.transform(fileRange);
		Option<CoreExpr> newContract = optTransform(contract, transformer);
		CoreExpr newBody = transformer.transform(body);
		List<FunArg> newArgs = ExprList.transformExprs(args, transformer);
		if(newRange == fileRange &&
				newContract == contract &&
				newBody == body &&
				newArgs == args)
			return this;
		return new FunctionLiteral(newRange, newArgs, newContract, newBody);
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitFunctionLiteral(this);
	}
}
