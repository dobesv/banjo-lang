package banjo.dom.core;

import org.eclipse.jdt.annotation.Nullable;

import fj.F;
import fj.Ord;
import fj.data.List;
import banjo.dom.Expr;
import banjo.dom.source.Operator;
import banjo.dom.source.Operator.Position;
import banjo.dom.source.Precedence;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.parser.util.AbstractCachedHashCode;
import banjo.parser.util.ExprOrd;
import banjo.parser.util.ListUtil;
import banjo.parser.util.SourceFileRange;

public class Method extends AbstractCoreExpr implements CoreExpr {
	
	private final List<Key> selfArg;
	private final List<Key> nameParts;
	private final List<List<List<Key>>> argumentLists;
	private final CoreExpr body;

	public static final Ord ORD = ExprOrd.<Method>exprOrd();
	
	public static final Key LOOKUP_METHOD_NAME = new Identifier(List.<SourceFileRange>nil(), Operator.LOOKUP.getOp());

	public Method(List<SourceFileRange> ranges, List<Key> selfArg, fj.data.List<Key> nameParts, List<List<List<Key>>> argumentLists, CoreExpr body) {
		super(calcHash(ranges, selfArg, nameParts, argumentLists, body), ranges);
		this.selfArg = selfArg;
		this.nameParts = nameParts;
		this.argumentLists = argumentLists;
		this.body = body;
	}

	public static Method nullary(Key name, CoreExpr body) {
		return new Method(List.<SourceFileRange>nil(), List.<Key>nil(), List.<Key>nil(), List.<List<List<Key>>>nil(), body);
	}
	private static int calcHash(List<SourceFileRange> ranges, List<Key> selfArg, List<Key> nameParts,
			List<List<List<Key>>> argumentLists, CoreExpr body) {
		final int prime = 31;
		int result = 1;
		result = prime * result + selfArg.hashCode();
		result = prime * result + nameParts.hashCode();
		result = prime * result + argumentLists.hashCode();
		result = prime * result + body.hashCode();
		result = prime * result + ranges.hashCode();
		return result;
	}

	@SuppressWarnings("null")
	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		toSource(sb);
		return sb.toString();
	}

	static void argToSource(List<Key> arg, StringBuffer sb) {
		while(arg.isNotEmpty()) {
			arg.head().toSource(sb);
			arg = arg.tail();
			if(arg.isNotEmpty())
				sb.append("()");
		}
		
	}
	public void toSource(final StringBuffer sb) {
		// Check for brackets, apply

		final boolean hasSelfName = this.hasSelfArg();
		final Operator operator = this.selfArg.length() != 1 || this.argumentLists.length() != 1 || this.argumentLists.head().length() != 1 || this.argumentLists.head().head().length() != 1 ? null 
				: this.nameParts.length() == 1
					? this.nameParts.head().equals(LOOKUP_METHOD_NAME) ? Operator.LOOKUP
					: this.argumentLists.head().length() == 1  ? Operator.fromMethodName(this.nameParts.head().getKeyString(), true) 
					: this.argumentLists.head().isEmpty() ? Operator.fromMethodName(this.nameParts.head().getKeyString(), false) 
					: null 
				: null;
		if(operator != null && operator.isInfix()) {
			sb.append('(');
			Key arg = argumentLists.head().head().head();
			if(operator.isParen()) {
				selfArg.head().toSource(sb);
				sb.append(operator.getParenType().getStartChar());
				arg.toSource(sb);
				sb.append(operator.getParenType().getEndChar());
			} else if(operator.isSelfOnRightMethodOperator()) {
				arg.toSource(sb);
				sb.append(' ');
				operator.toSource(sb);
				sb.append(' ');
				this.selfArg.head().toSource(sb);
			} else {
				this.selfArg.head().toSource(sb);
				sb.append(' ');
				operator.toSource(sb);
				sb.append(' ');
				arg.toSource(sb);
			}
			sb.append(')');
		} else if(operator != null && operator.isPrefix()) {
			sb.append('(');
			operator.toSource(sb);
			this.selfArg.head().toSource(sb);
			sb.append(')');
		} else if(operator != null && operator.isSuffix()) {
			sb.append('(');
			this.selfArg.head().toSource(sb);
			operator.toSource(sb);
			sb.append(')');
		} else {
			
			if(hasSelfName) {
				argToSource(selfArg, sb);
				sb.append('.');
			} else if(this.nameParts.length() == 1 && this.nameParts.head().equals(this.body) && this.argumentLists.isEmpty()) {
				this.body.toSource(sb);
				return;
			}
			
			List<Key> np = nameParts;
			List<List<List<Key>>> al = argumentLists;
			while(np.isNotEmpty()) {
				np.head().toSource(sb);
				np = np.tail();
				if(al.isNotEmpty()) {
					sb.append('(');
					for(List<Key> arg : al.head()) {
						if(arg == null) throw new NullPointerException();
						argToSource(arg, sb);
					}
					sb.append(')');
					al = al.tail();
				} else if(np.isNotEmpty()) {
					sb.append("()");
				}
			}
		}
		sb.append(' ');
		Operator.ASSIGNMENT.toSource(sb);
		sb.append(' ');
		this.body.toSource(sb, Precedence.COLON);
	}

	public CoreExpr getBody() {
		return this.body;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (!(obj instanceof Method))
			return false;
		final Method other = (Method) obj;
		if (!this.nameParts.equals(other.nameParts))
			return false;
		if (!this.body.equals(other.body))
			return false;
		if (!this.selfArg.equals(other.selfArg))
			return false;
		if (!this.argumentLists.equals(other.argumentLists))
			return false;
		return true;
	}

	@Override
	public int compareTo(@Nullable Expr o) {
		if(o == null) return -1;
		int cmp = getClass().getName().compareTo(o.getClass().getName());
		if(cmp == 0) {
			Method other = (Method) o;
			cmp = nameParts.compare(Key.ORD, other.nameParts).toPlusOrMinusOne();
			if(cmp == 0) cmp = argumentLists.compare(Ord.listOrd(Ord.listOrd(Key.ORD)), other.argumentLists).toPlusOrMinusOne();
			if(cmp == 0) cmp = selfArg.compare(Key.ORD, other.selfArg).toPlusOrMinusOne();
			if(cmp == 0) cmp = this.body.compareTo(other.body);
			if(cmp == 0) cmp = ListUtil.compare(this.getSourceFileRanges(), other.getSourceFileRanges());
		}
		return cmp;
	}

	public boolean hasSelfArg() {
		return !this.selfArg.isEmpty();
	}

	/**
	 * True if this method could have been created using the lambda syntax (x,...) -> y
	 */
	public boolean isSimpleApplyMethod() {
		return this.nameParts.isEmpty();
	}

	@Override
	public Precedence getPrecedence() {
		return Precedence.ASSIGNMENT;
	}

	@Override
	public <T> T acceptVisitor(CoreExprVisitor<T> visitor) {
		return visitor.method(getSourceFileRanges(), selfArg, nameParts, argumentLists, body);
	}

	@Override
	public <T> T acceptVisitor(final CoreExprAlgebra<T> visitor) {
		return visitor.method(getSourceFileRanges(),
				selfArg.map(new F<Key, T>() {
					@Override
					public T f(@Nullable Key a) {
						if(a == null) throw new NullPointerException();
						return a.acceptVisitor(visitor);
					}
				}),
				nameParts.map(new F<Key, T>() {
					@Override
					public T f(@Nullable Key a) {
						if(a == null) throw new NullPointerException();
						return a.acceptVisitor(visitor);
					}
				}),
				argumentLists.map(new F<List<List<Key>>, List<List<T>>>() {
					@Override
					public List<List<T>> f(@Nullable List<List<Key>> a) {
						if(a == null) throw new NullPointerException();
						return a.map(new F<List<Key>, List<T>>() {
							@Override
							public List<T> f(@Nullable List<Key> a) {
								if(a == null) throw new NullPointerException();
								return a.map(new F<Key, T>() {
									@Override
									public T f(@Nullable Key a) {
										if(a == null) throw new NullPointerException();
										return a.acceptVisitor(visitor);
									}
								});
							}
						});
					}
				}),
				body.acceptVisitor(visitor));
	}

	public List<Key> getSelfArg() {
		return selfArg;
	}

	public List<Key> getNameParts() {
		return nameParts;
	}

	public List<List<List<Key>>> getArgumentLists() {
		return argumentLists;
	}

	

}