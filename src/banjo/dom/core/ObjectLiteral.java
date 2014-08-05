package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.dom.token.Key;
import banjo.dom.token.StringLiteral;
import banjo.parser.BanjoScanner;
import banjo.parser.util.ExprOrd;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;
import fj.F;
import fj.Ord;
import fj.Unit;
import fj.data.List;
import fj.data.TreeMap;


public class ObjectLiteral extends AbstractCoreExpr implements CoreExpr {
	public static final ObjectLiteral EMPTY = new ObjectLiteral(List.<SourceFileRange>nil());
	public static final fj.data.List<Method> EMPTY_METHOD_LIST = nonNull(fj.data.List.<Method>nil());
	private final fj.data.List<Method> methods;

	public static final Ord<Method> ORD = ExprOrd.<Method>exprOrd();

	public ObjectLiteral(List<SourceFileRange> ranges, fj.data.List<Method> fields) {
		super(fields.hashCode()+ranges.hashCode(), ranges);
		this.methods = nonNull(fields);
	}

	@SafeVarargs
	public ObjectLiteral(List<SourceFileRange> ranges, Method ... methods) {
		this(ranges, fj.data.List.list(methods));
	}

	public ObjectLiteral(Method ... methods) {
		this(List.<SourceFileRange>nil(), fj.data.List.list(methods));
	}

	public fj.data.List<Method> getMethods() {
		return this.methods;
	}

	@Override
	public Precedence getPrecedence() {
		return isLambda() ? Precedence.FUNCTION : Precedence.ATOM;
	}

	@Override
	public void toSource(final StringBuffer sb) {
		if(isLambda()) {
			final Method method = this.methods.head();
			if(method.hasSelfArg() || !method.getArgumentLists().head().isEmpty()) {
				if(method.hasSelfArg()) {
					method.getSelfArg().acceptVisitor(new BaseCoreExprVisitor<Unit>() {
						@Override
						public Unit mixfixFunctionIdentifier(MixfixFunctionIdentifier mixfixFunctionIdentifier) {
							mixfixFunctionIdentifier.toSource(sb, method.getArgumentLists());
							return Unit.unit();
						}

						@Override
						public Unit key(Key key) {
							key.toSource(sb);
							argListToSource(sb, method);
							return Unit.unit();
						}

						@Override
						public Unit fallback() {
							throw new IllegalStateException();
						}
					});
				} else {
					// Note: Only one argument list is supported in this case, if there are others they are ignored as they would be invalid
					argListToSource(sb, method);
				}
				sb.append(' ');
			}
			Operator.FUNCTION.toSource(sb);
			sb.append(' ');
			method.getBody().toSource(sb, Operator.FUNCTION.getPrecedence());
		} else {
			sb.append('{');
			boolean first = true;
			for(final Method method : this.methods) {
				if(first) first = false;
				else sb.append(", ");
				method.toSource(sb);
			}
			sb.append('}');
		}
	}

	private void argListToSource(final StringBuffer sb, final Method method) {
		sb.append('(');
		boolean first = true;
		for(Key arg : method.getArgumentLists().head()) {
			if(first) first = false;
			else sb.append(", ");
			arg.toSource(sb);
		}
		sb.append(')');
	}

	public boolean isLambda() {
		return this.methods.isSingle() && this.methods.head().isSimpleApplyMethod();
	}

	public static StringBuffer maybeQuoteKey(String identifier, StringBuffer sb) {
		for(int i=0; i < identifier.length(); i++) {
			final int cp = identifier.codePointAt(i);
			if(cp > Character.MAX_VALUE) i++; // Actually a pair of characters
			final boolean ok = i==0 ? BanjoScanner.isIdentifierStart(cp):BanjoScanner.isIdentifierPart(cp);
			if(!ok) {
				return StringLiteral.toSource(identifier, sb);
			}
		}
		sb.append(identifier);
		return sb;
	}


	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.objectLiteral(this);
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof ObjectLiteral))
			return false;
		final ObjectLiteral other = (ObjectLiteral) obj;
		if (!this.methods.equals(other.methods))
			return false;
		return true;
	}

	@Override
	public int compareTo(@Nullable Expr o) {
		if(this == o)
			return 0;
		if(o == null)
			return -1;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final ObjectLiteral other = (ObjectLiteral) o;
			cmp = this.methods.compare(Method.ORD, other.methods).toInt();
			if(cmp == 0) cmp = super.compareTo(other);
		}
		return cmp;
	}

	public boolean isLazyValue() {
		return isLambda() && (this.methods.head().getArgumentLists().head().isEmpty());
	}

	@Override
	public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
		return visitor.objectLiteral(getSourceFileRanges(), methods.map(new F<Method, T>() {
			@Override
			public T f(@Nullable Method a) {
				if(a == null) throw new NullPointerException();
				return a.acceptVisitor(visitor);
			}
		}));
	}
}
