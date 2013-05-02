package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.AbstractExpr;
import banjo.dom.Expr;
import banjo.dom.ExprTransformer;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.Key;
import banjo.parser.util.FileRange;

public class FunctionLiteral extends AbstractCoreExpr implements CoreExpr {

	private final List<FunArg> args;
	private final @Nullable CoreExpr contract;
	private final CoreExpr body;
	private final @Nullable Key selfName;
	
	public static final @Nullable CoreExpr CONTRACT_NONE = null;
	
	public FunctionLiteral(SourceExpr sourceExpr, @Nullable Key selfName, List<FunArg> args, @Nullable CoreExpr contract, CoreExpr body) {
		super(sourceExpr);
		this.args = nonNull(Collections.unmodifiableList(args));
		this.contract = contract;
		this.body = body;
		this.selfName = selfName;
	}
	
	public FunctionLiteral(SourceExpr sourceExpr, List<FunArg> args, @Nullable CoreExpr contract, CoreExpr body) {
		this(sourceExpr, null, args, contract, body);
	}
	
	/**
	 * Lazy expression which may have an operator in front, thus the specific text range
	 */
	public FunctionLiteral(SourceExpr sourceExpr, CoreExpr body) {
		this(sourceExpr, nonNull(Collections.<FunArg>emptyList()), CONTRACT_NONE, body);
	}

	/**
	 * Lazy expression with no operator in front
	 */
	public FunctionLiteral(CoreExpr body) {
		this(body.getSourceExpr(), nonNull(Collections.<FunArg>emptyList()), CONTRACT_NONE, body);
	}
	/**
	 * Easy unary function
	 */
	public FunctionLiteral(SourceExpr sourceExpr, FunArg arg, CoreExpr body) {
		this(sourceExpr, nonNull(Collections.singletonList(arg)), CONTRACT_NONE, body);
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
		if(selfName != null) {
			// TODO ... this syntax won't parse back in again!
			nonNull(selfName).toSource(sb);
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
		
		if(contract != null) {
			sb.append(" : ");
			nonNull(contract).toSource(sb, Precedence.FUNCTION);
		}
		sb.append(" -> ");
		
		body.toSource(sb, Precedence.FUNCTION);
	}

	public @Nullable CoreExpr getContract() {
		return contract;
	}
	
	public boolean hasContract() {
		return contract != null;
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitFunctionLiteral(this);
	}

	public @Nullable Key getSelfName() {
		return selfName;
	}
}
