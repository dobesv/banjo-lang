package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.Precedence;
import banjo.dom.token.StringLiteral;
import banjo.parser.BanjoScanner;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
import fj.data.TreeMap;


public class ObjectLiteral extends AbstractCoreExpr implements CoreExpr {
	public static final ObjectLiteral EMPTY = new ObjectLiteral(SourceFileRange.SYNTHETIC);
	public static final TreeMap<String, fj.data.List<Method>> EMPTY_METHOD_MAP = nonNull(TreeMap.<String, fj.data.List<Method>>empty(Ord.stringOrd));
	public static final fj.data.List<Method> EMPTY_METHOD_LIST = nonNull(fj.data.List.<Method>nil());
	private final fj.data.List<Method> methods;

	public ObjectLiteral(SourceFileRange sfr, fj.data.List<Method> fields) {
		super(fields.hashCode()+sfr.hashCode(), sfr);
		this.methods = nonNull(fields);
	}

	@SafeVarargs
	public ObjectLiteral(SourceFileRange sfr, Method ... methods) {
		this(sfr, fj.data.List.list(methods));
	}

	public fj.data.List<Method> getMethods() {
		return this.methods;
	}

	@Override
	public Precedence getPrecedence() {
		return isLambda() ? Precedence.FUNCTION : Precedence.ATOM;
	}

	@Override
	public void toSource(StringBuffer sb) {
		if(isLambda()) {
			final Method method = this.methods.head();
			if(method.hasSelfName() || !method.getArgs().isEmpty()) {
				if(method.hasSelfName()) method.getSelfName().toSource(sb);
				method.formalArgListToSource(sb);
				sb.append(' ');
			}
			Operator.FUNCTION.toSource(sb);
			sb.append(' ');
			method.getBody().toSource(sb, Operator.FUNCTION.getPrecedence());
		} else {
			sb.append('{');
			boolean first = true;
			for(final Method f : this.methods) {
				if(first) first = false;
				else sb.append(", ");
				f.toSource(sb);
			}
			sb.append('}');
		}
	}

	public boolean isLambda() {
		return this.methods.length() == 1 &&
				this.methods.head().isSimpleApplyMethod();
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
	public @Nullable <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
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
			cmp = ListUtil.compare(this.methods, other.methods);
			if(cmp == 0) cmp = super.compareTo(other);
		}
		return cmp;
	}

	public boolean isLazyValue() {
		return isLambda() && (this.methods.head().getArgs().isEmpty());
	}
}
