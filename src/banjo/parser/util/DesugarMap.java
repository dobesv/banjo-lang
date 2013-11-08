package banjo.parser.util;

import static banjo.parser.util.Check.nonNull;

import java.util.Objects;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.CoreExpr;
import banjo.dom.core.Method;
import banjo.dom.core.MethodParamDecl;
import banjo.dom.source.SourceExpr;
import fj.F;
import fj.Ord;
import fj.P2;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

/**
 * Provide a way to figure out the source file range of a CoreExpr.
 */
public class DesugarMap {
	private final TreeMap<CoreExpr, fj.data.Set<SourceExpr>> exprs;
	private final TreeMap<Method, fj.data.Set<MethodSourceExprs>> methods;
	private final TreeMap<MethodParamDecl, fj.data.Set<SourceExpr>> params;

	@SuppressWarnings("null")
	static final fj.data.Set<SourceExpr> EMPTY_SOURCE_EXPR_SET = fj.data.Set.empty(SourceExpr.ORD);
	@SuppressWarnings("null")
	static final fj.data.Set<MethodSourceExprs> EMPTY_METHOD_SOURCE_EXPRS_SET = fj.data.Set.empty(Ord.<MethodSourceExprs>comparableOrd());
	@SuppressWarnings("null")
	static final fj.data.TreeMap<CoreExpr, fj.data.Set<SourceExpr>> EMPTY_CORE_EXPR_TO_SOURCE_EXPR_MAP = TreeMap.empty(CoreExpr.ORD);
	@SuppressWarnings("null")
	static final fj.data.TreeMap<Method, fj.data.Set<MethodSourceExprs>> EMPTY_METHOD_DEF_TO_SOURCE_EXPR_MAP = TreeMap.empty(Ord.<Method>comparableOrd());
	@SuppressWarnings("null")
	static final fj.data.TreeMap<MethodParamDecl, fj.data.Set<SourceExpr>> EMPTY_PARAM_DECL_TO_SOURCE_EXPR_MAP = TreeMap.empty(Ord.<MethodParamDecl>comparableOrd());

	static class MethodSourceExprs implements Comparable<MethodSourceExprs> {
		private final SourceExpr method;
		@Nullable
		private final SourceExpr signature;
		@Nullable
		private final SourceExpr body;
		public MethodSourceExprs(SourceExpr method, @Nullable SourceExpr signature,
				@Nullable SourceExpr body) {
			super();
			this.method = method;
			this.signature = signature;
			this.body = body;
		}
		public SourceExpr getMethod() {
			return this.method;
		}
		public @Nullable SourceExpr getSignature() {
			return this.signature;
		}
		public @Nullable SourceExpr getBody() {
			return this.body;
		}
		@Override
		public int hashCode() {
			return Objects.hash(this.method, this.body, this.signature);
		}
		@Override
		public boolean equals(@Nullable Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (!(obj instanceof MethodSourceExprs))
				return false;
			final MethodSourceExprs other = (MethodSourceExprs) obj;
			return this.method.equals(other.method) &&
					Objects.equals(this.signature, other.signature) &&
					Objects.equals(this.body, other.body);
		}
		@Override
		public int compareTo(MethodSourceExprs o) {
			int cmp = this.method.compareTo(o.method);
			if(cmp == 0) cmp = Objects.compare(this.signature, o.signature, SourceExpr.COMPARATOR);
			if(cmp == 0) cmp = Objects.compare(this.body, o.body, SourceExpr.COMPARATOR);
			return cmp;
		}
		@Override
		public String toString() {
			return "[method=" + this.method + ", signature="	+ this.signature + ", body=" + this.body + "]";
		}


	}
	public DesugarMap() {
		this(EMPTY_CORE_EXPR_TO_SOURCE_EXPR_MAP,
				EMPTY_METHOD_DEF_TO_SOURCE_EXPR_MAP,
				EMPTY_PARAM_DECL_TO_SOURCE_EXPR_MAP);
	}

	public DesugarMap(TreeMap<CoreExpr, Set<SourceExpr>> coreExprToSourceExpr,
			TreeMap<Method, Set<MethodSourceExprs>> methodToSourceExprs,
			TreeMap<MethodParamDecl, Set<SourceExpr>> paramToSourceExpr) {
		super();
		this.exprs = coreExprToSourceExpr;
		this.methods = methodToSourceExprs;
		this.params = paramToSourceExpr;
	}

	/**
	 * Find the file range for a given method within certain bounds.
	 * 
	 * If there's no match, returns the same bounds as given.
	 */
	public FileRange getFirstMethodRangeIn(SourceMap sourceMap, FileRange bounds, Method method) {
		@NonNull @SuppressWarnings("null")
		final Option<Set<SourceExpr>> candidates = this.methods.get(method).map(new F<Set<MethodSourceExprs>, Set<SourceExpr>>() {
			@Override
			public Set<SourceExpr> f(Set<MethodSourceExprs> a) {
				return a.map(SourceExpr.ORD, new F<MethodSourceExprs, SourceExpr>() {
					@Override
					public SourceExpr f(MethodSourceExprs a) {
						return a.method;
					}
				});
			}
		});
		return getRangeIn(sourceMap, bounds, candidates, true);
	}

	/**
	 * Find the file range for a given method's signature within certain bounds (should be the bounds for the method)
	 * 
	 * If there's no match, returns the same bounds as given.
	 */
	public FileRange getFirstMethodSignatureRangeIn(SourceMap sourceMap, FileRange bounds, Method method) {
		@NonNull @SuppressWarnings("null")
		final Option<Set<SourceExpr>> candidates = this.methods.get(method).map(new F<Set<MethodSourceExprs>, Set<SourceExpr>>() {
			@Override
			public Set<SourceExpr> f(Set<MethodSourceExprs> a) {
				return a.filter(new F<DesugarMap.MethodSourceExprs, Boolean>() {
					@Override
					public Boolean f(MethodSourceExprs a) {
						return a.signature != null;
					}
				}).map(SourceExpr.ORD, new F<MethodSourceExprs, SourceExpr>() {
					@Override
					public @Nullable SourceExpr f(MethodSourceExprs a) {
						return a.signature;
					}
				});
			}
		});
		return getRangeIn(sourceMap, bounds, candidates, true);
	}

	/**
	 * Find the file range for a given method's signature within certain bounds (should be the bounds for the method)
	 * 
	 * If there's no match, returns the same bounds as given.
	 */
	public FileRange getLastMethodBodyRangeIn(SourceMap sourceMap, FileRange bounds, Method method) {
		@NonNull @SuppressWarnings("null")
		final Option<Set<SourceExpr>> candidates = this.methods.get(method).map(new F<Set<MethodSourceExprs>, Set<SourceExpr>>() {
			@Override
			public Set<SourceExpr> f(Set<MethodSourceExprs> a) {
				return a.filter(new F<DesugarMap.MethodSourceExprs, Boolean>() {
					@Override
					public Boolean f(MethodSourceExprs a) {
						return a.body != null;
					}
				}).map(SourceExpr.ORD, new F<MethodSourceExprs, SourceExpr>() {
					@Override
					public @Nullable SourceExpr f(MethodSourceExprs a) {
						return a.body;
					}
				});
			}
		});
		return getRangeIn(sourceMap, bounds, candidates, false);
	}


	/**
	 * Find the file range of the first occurrence of the given method parameter within the given bounds.
	 * 
	 * If there's no match, returns the same bounds as given.
	 */
	public FileRange getFirstRangeIn(SourceMap sourceMap, FileRange bounds, MethodParamDecl methodParamDecl) {
		return getRangeIn(sourceMap, bounds, nonNull(this.params.get(methodParamDecl)), true);
	}

	/**
	 * Find the file range of the first occurrence of the given core expression within the given bounds.
	 * 
	 * If there's no match, returns the same bounds as given.
	 */
	public FileRange getRangeIn(SourceMap sourceMap, FileRange bounds, CoreExpr expr, boolean first) {
		Option<Set<SourceExpr>> candidateSet = nonNull(this.exprs.get(expr));
		candidateSet = orSameSourceExpr(expr, candidateSet);
		return getRangeIn(sourceMap, bounds, candidateSet, first);
	}

	private FileRange getRangeIn(SourceMap sourceMap, FileRange bounds, final Option<Set<SourceExpr>> candidateSet, boolean first) {
		FileRange result = bounds;
		for(final SourceExpr node : candidateSet.orSome(EMPTY_SOURCE_EXPR_SET)) {
			for(final FileRange candidate : sourceMap.get(nonNull(node))) {
				if(!candidate.isSubrange(bounds))
					continue;
				if(result == bounds || (first ? candidate.getStart().before(result.getStart()) : result.getStart().before(candidate.getStart()))) {
					result = candidate;
				}
			}
		}
		return result;
	}

	public Option<Set<SourceExpr>> orSameSourceExpr(CoreExpr expr,
			Option<Set<SourceExpr>> candidateSet) {
		// For nodes that are both a core and source expr (like identifiers, string/number literals) we don't
		// put them in the map unless they actually mapped to or from something different than they
		// started (which they typically do not).
		if(expr instanceof SourceExpr) {
			candidateSet = Option.some(candidateSet.orSome(EMPTY_SOURCE_EXPR_SET).insert((SourceExpr)expr));
		}
		return candidateSet;
	}


	public DesugarMap insert(CoreExpr expr, SourceExpr sourceExpr) {
		if(expr.equals(sourceExpr))
			return this; // Don't bother
		final TreeMap<CoreExpr, Set<SourceExpr>> newSourceExprMap = nonNull(this.exprs.set(expr, this.exprs.get(expr).orSome(EMPTY_SOURCE_EXPR_SET).insert(sourceExpr)));
		return new DesugarMap(newSourceExprMap, this.methods, this.params);
	}

	/**
	 * Add a method mapping
	 * 
	 * @param expr Method to use as the key
	 * @param methodSourceExpr Root expression for the method, should encompass the signature and body
	 * @param signatureSourceExpr Body expression for the method (may be null for a synthetic method body)
	 * @param bodySourceExpr Signature expression for the method (may be null for a synthetic method signature)
	 * @return A new DesugarMap
	 */
	public DesugarMap insert(Method expr, SourceExpr methodSourceExpr, @Nullable SourceExpr signatureSourceExpr, @Nullable SourceExpr bodySourceExpr) {
		@NonNull @SuppressWarnings("null")
		final TreeMap<Method, Set<MethodSourceExprs>> newMethods = this.methods.set(expr, this.methods.get(expr).orSome(EMPTY_METHOD_SOURCE_EXPRS_SET).insert(new MethodSourceExprs(methodSourceExpr, signatureSourceExpr, bodySourceExpr)));
		return new DesugarMap(this.exprs, newMethods, this.params);
	}

	public DesugarMap insert(MethodParamDecl methodParamDecl, SourceExpr sourceExpr) {
		@NonNull @SuppressWarnings("null")
		final TreeMap<MethodParamDecl, Set<SourceExpr>> newParamDeclToSourceExpr = this.params.set(methodParamDecl, this.params.get(methodParamDecl).orSome(EMPTY_SOURCE_EXPR_SET).insert(sourceExpr));
		return new DesugarMap(this.exprs, this.methods, newParamDeclToSourceExpr);
	}

	public TreeMap<CoreExpr, fj.data.Set<SourceExpr>> getExprs() {
		return this.exprs;
	}

	public TreeMap<MethodParamDecl, fj.data.Set<SourceExpr>> getParams() {
		return this.params;
	}

	@SuppressWarnings("null")
	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("DesugarMap:\n");
		for(final P2<CoreExpr, Set<SourceExpr>> p : this.exprs) {
			for(final SourceExpr sourceExpr : p._2()) {
				sb.append("    ");
				sb.append(sourceExpr.toString());
				sb.append(" --> ");
				sb.append(p._1().toSource());
				sb.append('\n');
			}
		}
		for(final P2<MethodParamDecl, Set<SourceExpr>> p : this.params) {
			for(final SourceExpr sourceExpr : p._2()) {
				sb.append("    ");
				sb.append(sourceExpr.toString());
				sb.append(" --> ");
				sb.append(p._1().toString());
				sb.append('\n');
			}
		}
		for(final P2<Method, Set<MethodSourceExprs>> p : this.methods) {
			for(final MethodSourceExprs sourceExpr : p._2()) {
				sb.append("    ");
				sb.append(sourceExpr.toString());
				sb.append(" --> ");
				sb.append(p._1().toString());
				sb.append('\n');
			}
		}
		return sb.toString();
	}


}
