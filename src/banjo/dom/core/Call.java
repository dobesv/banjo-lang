package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import java.util.Arrays;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;

public class Call extends AbstractCoreExpr implements CoreExpr {

	private final CoreExpr callee;
	private final List<CoreExpr> arguments;

	public Call(SourceExpr sourceExpr, CoreExpr callee, List<CoreExpr> arguments) {
		this(sourceExpr.getSourceLength(), callee, arguments);
	}

	public Call(int sourceLength, CoreExpr callee, List<CoreExpr> arguments) {
		super(sourceLength, callee.hashCode() + arguments.hashCode());
		this.callee = callee;
		this.arguments = arguments;
	}

	public Call(SourceExpr sourceExpr, CoreExpr callee, CoreExpr ... arguments) {
		this(sourceExpr, callee, nonNull(Arrays.asList(arguments)));
	}

	public Call(int sourceLength, CoreExpr callee, CoreExpr ... arguments) {
		this(sourceLength, callee, nonNull(Arrays.asList(arguments)));
	}


	public CoreExpr getCallee() {
		return this.callee;
	}

	public List<CoreExpr> getArguments() {
		return this.arguments;
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.SUFFIX;
	}

	@Override
	public void toSource(StringBuffer sb) {
		this.callee.toSource(sb, Precedence.SUFFIX);
		sb.append('(');
		boolean first = true;
		for(final CoreExpr arg : this.arguments) {
			if(first) first = false;
			else sb.append(", ");
			arg.toSource(sb, Precedence.COMMA.nextHighest());
		}
		sb.append(')');
	}


	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.call(this);
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof Call))
			return false;
		final Call other = (Call) obj;
		if (!this.arguments.equals(other.arguments))
			return false;
		if (!this.callee.equals(other.callee))
			return false;
		return true;
	}

}
