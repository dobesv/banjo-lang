package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;

public class FunctionLiteral extends AbstractCoreExpr implements CoreExpr {

	private final List<FunArg> args;
	private final CoreExpr guarantee;
	private final CoreExpr body;
	private final Key selfName;

	public static final CoreExpr DEFAULT_GUARANTEE = FunArg.NO_ASSERTION;
	public static final Key DEFAULT_SELF_NAME = new Identifier("_self");

	public FunctionLiteral(SourceExpr sourceExpr, Key selfName, List<FunArg> args, CoreExpr contract, CoreExpr body) {
		this(sourceExpr.getSourceLength(), selfName, args, contract, body);
	}
	public FunctionLiteral(int sourceLength, Key selfName, List<FunArg> args, CoreExpr contract, CoreExpr body) {
		super(sourceLength, selfName.hashCode() + args.hashCode() + contract.hashCode() + body.hashCode());
		this.args = nonNull(Collections.unmodifiableList(args));
		this.guarantee = contract;
		this.body = body;
		this.selfName = selfName;
	}

	public FunctionLiteral(int sourceLength, List<FunArg> args, CoreExpr contract, CoreExpr body) {
		this(sourceLength, DEFAULT_SELF_NAME, args, contract, body);
	}

	/**
	 * Lazy expression which may have an operator in front, thus the specific text range
	 */
	public FunctionLiteral(SourceExpr sourceExpr, CoreExpr body) {
		this(sourceExpr.getSourceLength(), nonNull(Collections.<FunArg>emptyList()), DEFAULT_GUARANTEE, body);
	}

	/**
	 * Lazy expression with no operator in front
	 */
	public FunctionLiteral(CoreExpr body) {
		this(body.getSourceLength(), nonNull(Collections.<FunArg>emptyList()), DEFAULT_GUARANTEE, body);
	}
	/**
	 * Easy unary function
	 */
	public FunctionLiteral(SourceExpr sourceExpr, FunArg arg, CoreExpr body) {
		this(sourceExpr.getSourceLength(), nonNull(Collections.singletonList(arg)), DEFAULT_GUARANTEE, body);
	}


	public List<FunArg> getArgs() {
		return this.args;
	}

	public CoreExpr getBody() {
		return this.body;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.FUNCTION;
	}

	@Override
	public void toSource(StringBuffer sb) {
		// Using getKeyString() here because the selfName might have an offset attached to it
		final boolean showSelfName = !this.selfName.equals(DEFAULT_SELF_NAME);
		final boolean showGuarantee = !this.guarantee.equals(DEFAULT_GUARANTEE);
		if(showSelfName || showGuarantee || !this.args.isEmpty()) {
			if(showSelfName) {
				nonNull(this.selfName).toSource(sb);
				sb.append(".");
			}
			sb.append('(');
			boolean first = true;
			for(final FunArg arg : this.args) {
				if(first) first = false;
				else sb.append(", ");
				arg.toSource(sb);
			}
			sb.append(')');
			if(showGuarantee) {
				sb.append(" : ");
				nonNull(this.guarantee).toSource(sb, Precedence.FUNCTION);
			}
			sb.append(" -> ");
		} else {
			// Lazy value
			sb.append("-> ");
		}

		this.body.toSource(sb, Precedence.FUNCTION);
	}

	public CoreExpr getGuarantee() {
		return this.guarantee;
	}

	public boolean hasContract() {
		return !this.guarantee.equals(DEFAULT_GUARANTEE);
	}

	public boolean hasSelfName() {
		return !this.selfName.equals(DEFAULT_SELF_NAME);
	}

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.functionLiteral(this);
	}

	public Key getSelfName() {
		return this.selfName;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof FunctionLiteral))
			return false;
		final FunctionLiteral other = (FunctionLiteral) obj;
		if (!this.args.equals(other.args))
			return false;
		if (!this.body.equals(other.body))
			return false;
		if (!this.guarantee.equals(other.guarantee))
			return false;
		if (!this.selfName.equals(other.selfName))
			return false;
		return true;
	}

}
