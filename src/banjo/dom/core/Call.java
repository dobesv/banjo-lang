package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import java.util.Arrays;
import java.util.List;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExpr;

public class Call extends AbstractCoreExpr implements CoreExpr {

	private final CoreExpr callee;
	private final List<CoreExpr> arguments;

	public Call(SourceExpr sourceExpr, CoreExpr callee, List<CoreExpr> arguments) {
		super(sourceExpr);
		this.callee = callee;
		this.arguments = arguments;
	}
	public Call(SourceExpr sourceExpr, CoreExpr callee, CoreExpr ... arguments) {
		this(sourceExpr, callee, nonNull(Arrays.asList(arguments)));
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
		for(final Expr arg : this.arguments) {
			if(first) first = false;
			else sb.append(", ");
			arg.toSource(sb, Precedence.COMMA.nextHighest());
		}
		sb.append(')');
	}


	@Override
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.visitCall(this);
	}
}
