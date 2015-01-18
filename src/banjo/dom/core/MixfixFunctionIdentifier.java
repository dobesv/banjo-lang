package banjo.dom.core;

import banjo.dom.BadExpr;
import banjo.dom.Expr;
import banjo.dom.source.Precedence;
import banjo.dom.source.SourceExprAlgebra;
import banjo.dom.source.SourceExprVisitor;
import banjo.dom.token.Key;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;
import fj.data.List;

public class MixfixFunctionIdentifier extends AbstractCoreExpr implements Key, CoreExpr {

	private final List<String> parts;

	public MixfixFunctionIdentifier(List<SourceFileRange> ranges, List<String> parts) {
		super(parts.hashCode(), ranges);
		if(parts.isEmpty() || parts.tail().isEmpty())
			throw new IllegalStateException("Mixfix identifier must have 2 or more parts");
		this.parts = parts;
	}

	@Override
	public void toSource(StringBuffer sb) {
		boolean first = true;
		for(String part : getParts()) {
			if(first) first = false;
			else sb.append("()");
			sb.append(part);
		}
	}

	public void toSource(StringBuffer sb, List<List<Key>> argumentLists) {
		List<String> nl = getParts();
		List<List<Key>> al = argumentLists;
		while(nl.isNotEmpty()) {
			sb.append(nl.head());
			nl = nl.tail();
			if(al.isNotEmpty()) {
				sb.append('(');
				boolean first = true;
				for(Key arg : al.head()) {
					if(first) first = false;
					else sb.append(", ");
					arg.toSource(sb);
				}
				sb.append(')');
				al = al.tail();
			} else if(nl.isNotEmpty()) {
				sb.append("()");
			}
		}
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ATOM;
	}

	@Override
	public <T> T acceptVisitor(SourceExprVisitor<T> visitor) {
		return visitor.mixfixFunctionIdentifier(this);
	}

	@Override
	public List<BadExpr> getProblems() {
		return List.nil();
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.mixfixFunctionIdentifier(this);
	}

	@Override
	public <T> T acceptVisitor(CoreExprAlgebra<T> visitor) {
		return visitor.mixfixFunctionIdentifier(getSourceFileRanges(), getParts());
	}

	@Override
	public <T> T acceptVisitor(SourceExprAlgebra<T> visitor) {
		throw new IllegalStateException("Not a really source expression");
	}

	public List<String> getParts() {
		return parts;
	}

	@Override
	public int compareTo(Expr o) {
		if(o == null) return -1;
		if(this == o)
			return 0;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			final MixfixFunctionIdentifier other = (MixfixFunctionIdentifier) o;
			if(cmp == 0) cmp = ListUtil.compare(this.parts, other.parts);
			if(cmp == 0) cmp = super.compareTo(o);
		}
		return cmp;
	}

	@Override
	public boolean equals(Object obj) {
		if(obj == this) return true;
		if(obj == null || !(obj instanceof MixfixFunctionIdentifier)) return false;
		if(!super.equals(obj)) return false;
		final MixfixFunctionIdentifier x = (MixfixFunctionIdentifier) obj;
		return x.parts.equals(this.parts);
	}

}
