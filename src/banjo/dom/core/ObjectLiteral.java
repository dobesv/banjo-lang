package banjo.dom.core;

import static banjo.parser.util.Check.nonNull;

import java.util.Arrays;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import banjo.dom.source.Precedence;
import banjo.dom.token.StringLiteral;
import banjo.parser.BanjoScanner;
import banjo.parser.util.ListUtil;
import fj.Ord;
import fj.data.List;
import fj.data.TreeMap;


public class ObjectLiteral extends AbstractCoreExpr implements CoreExpr {

	public static final TreeMap<String, fj.data.List<Method>> EMPTY_METHOD_MAP = nonNull(TreeMap.<String, fj.data.List<Method>>empty(Ord.stringOrd));
	public static final fj.data.List<Method> EMPTY_METHOD_LIST = nonNull(fj.data.List.<Method>nil());
	private final Iterable<Method> methods;
	@Nullable
	private TreeMap<String, fj.data.List<Method>> methodMap;

	public ObjectLiteral(Iterable<Method> fields) {
		super(fields.hashCode());
		this.methods = nonNull(fields);
		this.methodMap = makeMethodMap(fields);
	}

	@SafeVarargs
	public ObjectLiteral(Method ... methods) {
		this(nonNull(Arrays.asList(methods)));
	}

	private static TreeMap<String, fj.data.List<Method>> makeMethodMap(Iterable<Method> methods) {
		TreeMap<String,fj.data.List<Method>> methodMap = EMPTY_METHOD_MAP;
		for(final Method method : methods) {
			final String k = method.getKey().getKeyString();
			methodMap = nonNull(methodMap.set(k, methodMap.get(k).orSome(EMPTY_METHOD_LIST).snoc(method)));
		}
		return methodMap;
	}

	public Iterable<Method> getMethods() {
		return this.methods;
	}

	public @Nullable Iterable<Method> getMethod(String name) {
		return getMethodMap().get(name).orSome(EMPTY_METHOD_LIST);
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public void toSource(StringBuffer sb) {
		sb.append('{');
		boolean first = true;
		for(final Method f : this.methods) {
			if(first) first = false;
			else sb.append(", ");
			f.toSource(sb);
		}
		sb.append('}');
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
	public int compareTo(Expr o) {
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final ObjectLiteral other = (ObjectLiteral) o;
			cmp = ListUtil.compare(this.methods, other.methods);
		}
		return cmp;
	}

	public TreeMap<String, fj.data.List<Method>> getMethodMap() {
		final TreeMap<String, List<Method>> methodMap = this.methodMap;
		if(methodMap == null)
			return this.methodMap = makeMethodMap(this.methods);
		else
			return methodMap;
	}

}
