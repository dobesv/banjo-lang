package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;

public class FunctionLiteral extends AbstractCoreExpr implements CoreExpr {

	private final List<FunArg> args;
	private final CoreExpr guarantee;
	private final CoreExpr body;

	public static final CoreExpr DEFAULT_GUARANTEE = FunArg.NO_ASSERTION;

	public FunctionLiteral(SourceExpr sourceExpr, List<FunArg> args, CoreExpr guarantee, CoreExpr body) {
		this(sourceExpr.getSourceLength(), args, guarantee, body);
	}
	public FunctionLiteral(int sourceLength, List<FunArg> args, CoreExpr guarantee, CoreExpr body) {
		super(sourceLength, args.hashCode() + guarantee.hashCode() + body.hashCode());
		this.args = nonNull(Collections.unmodifiableList(args));
		this.guarantee = guarantee;
		this.body = body;
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
		final boolean showGuarantee = !this.guarantee.equals(DEFAULT_GUARANTEE);
		if(showGuarantee || !this.args.isEmpty()) {
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

	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.functionLiteral(this);
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
		return true;
	}

	public static boolean isFunctionLiteral(CoreExpr expr) {
		return nonNull(expr.acceptVisitor(new BaseCoreExprVisitor<Boolean>() {
			@Override
			@Nullable
			public Boolean functionLiteral(FunctionLiteral n) {
				return true;
			}

			@Override
			@Nullable
			public Boolean fallback(CoreExpr unsupported) {
				return false;
			}
		})).booleanValue();
	}
}
