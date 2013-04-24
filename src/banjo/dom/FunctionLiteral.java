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
	private final Option<Key> selfName;
	
	public static final Option<CoreExpr> CONTRACT_NONE = Option.none();
	
	public FunctionLiteral(FileRange range, Option<Key> selfName, List<FunArg> args, Option<CoreExpr> contract, CoreExpr body) {
		super(range);
		this.args = Collections.unmodifiableList(args);
		this.contract = contract;
		this.body = body;
		this.selfName = selfName;
	}
	
	public FunctionLiteral(FileRange range, List<FunArg> args, Option<CoreExpr> contract, CoreExpr body) {
		this(range, Key.NONE, args, contract, body);
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

	public CoreExpr getBody() {
		return body;
	}
	
	@Override
	public Precedence getPrecedence() {
		return Precedence.FUNCTION;
	}
	
	@Override
	public void toSource(StringBuffer sb) {
		if(selfName.isSome()) {
			// TODO ... this syntax won't parse back in again!
			selfName.some().toSource(sb);
			sb.append(".");
		}
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

	public Option<CoreExpr> getContract() {
		return contract;
	}
	
	public boolean hasContract() {
		return contract.isSome();
	}

	@Override
	public Expr transform(ExprTransformer transformer) {
		FileRange newRange = transformer.transform(fileRange);
		Option<CoreExpr> newContract = optTransform(contract, transformer);
		Option<Key> newSelfName = optTransform(selfName, transformer);
		CoreExpr newBody = transformer.transform(body);
		List<FunArg> newArgs = ExprList.transformExprs(args, transformer);
		if(newRange == fileRange &&
				newSelfName == selfName &&
				newContract == contract &&
				newBody == body &&
				newArgs == args)
			return this;
		return new FunctionLiteral(newRange, newSelfName, newArgs, newContract, newBody);
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitFunctionLiteral(this);
	}

	public Option<Key> getSelfName() {
		return selfName;
	}
}
