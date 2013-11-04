package banjo.parser.util;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.NonNull;

import banjo.dom.core.CoreExpr;
import banjo.dom.core.Method;
import banjo.dom.core.MethodParamDecl;
import banjo.dom.source.SourceExpr;
import fj.Ord;
import fj.P2;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

/**
 * Provide a way to figure out the source file range of a CoreExpr.
 */
public class DesugarMap {
	private final TreeMap<CoreExpr, fj.data.Set<SourceExpr>> coreExprToSourceExpr;
	private final TreeMap<Method, fj.data.Set<SourceExpr>> methodSignatureToSourceExpr;
	private final TreeMap<Method, fj.data.Set<SourceExpr>> methodBodyToSourceExpr;
	private final TreeMap<MethodParamDecl, fj.data.Set<SourceExpr>> paramDeclToSourceExpr;

	@SuppressWarnings("null")
	static final fj.data.Set<SourceExpr> EMPTY_SOURCE_EXPR_SET = fj.data.Set.empty(SourceExpr.ORD);
	@SuppressWarnings("null")
	static final fj.data.TreeMap<CoreExpr, fj.data.Set<SourceExpr>> EMPTY_CORE_EXPR_TO_SOURCE_EXPR_MAP = TreeMap.empty(CoreExpr.ORD);
	@SuppressWarnings("null")
	static final fj.data.TreeMap<Method, fj.data.Set<SourceExpr>> EMPTY_METHOD_DEF_TO_SOURCE_EXPR_MAP = TreeMap.empty(Ord.<Method>comparableOrd());
	@SuppressWarnings("null")
	static final fj.data.TreeMap<MethodParamDecl, fj.data.Set<SourceExpr>> EMPTY_PARAM_DECL_TO_SOURCE_EXPR_MAP = TreeMap.empty(Ord.<MethodParamDecl>comparableOrd());

	public DesugarMap() {
		this(EMPTY_CORE_EXPR_TO_SOURCE_EXPR_MAP,
				EMPTY_METHOD_DEF_TO_SOURCE_EXPR_MAP,
				EMPTY_METHOD_DEF_TO_SOURCE_EXPR_MAP,
				EMPTY_PARAM_DECL_TO_SOURCE_EXPR_MAP);
	}

	public DesugarMap(TreeMap<CoreExpr, Set<SourceExpr>> coreExprToSourceExpr,
			TreeMap<Method, Set<SourceExpr>> methodSignatureToSourceExpr,
			TreeMap<Method, Set<SourceExpr>> methodBodyToSourceExpr,
			TreeMap<MethodParamDecl, Set<SourceExpr>> paramToSourceExpr) {
		super();
		this.coreExprToSourceExpr = coreExprToSourceExpr;
		this.methodSignatureToSourceExpr = methodSignatureToSourceExpr;
		this.methodBodyToSourceExpr = methodBodyToSourceExpr;
		this.paramDeclToSourceExpr = paramToSourceExpr;
	}

	/**
	 * Find the file range of the first occurrence of the given method's signature within the given bounds.
	 * 
	 * If there's no match, returns the same bounds as given.
	 */
	public FileRange getFirstSignatureRangeIn(SourceMap sourceMap, FileRange bounds, Method method) {
		return getFirstRangeIn(sourceMap, bounds, nonNull(this.methodSignatureToSourceExpr.get(method)));
	}

	/**
	 * Find the file range of the first occurrence of the given method's body within the given bounds.
	 * 
	 * If there's no match, returns the same bounds as given.
	 */
	public FileRange getFirstBodyRangeIn(SourceMap sourceMap, FileRange bounds, Method method) {
		return getFirstRangeIn(sourceMap, bounds, nonNull(this.methodBodyToSourceExpr.get(method)));
	}

	/**
	 * Find the file range of the first occurrence of the given method parameter within the given bounds.
	 * 
	 * If there's no match, returns the same bounds as given.
	 */
	public FileRange getFirstRangeIn(SourceMap sourceMap, FileRange bounds, MethodParamDecl methodParamDecl) {
		return getFirstRangeIn(sourceMap, bounds, nonNull(this.paramDeclToSourceExpr.get(methodParamDecl)));
	}

	/**
	 * Find the file range of the first occurrence of the given core expression within the given bounds.
	 * 
	 * If there's no match, returns the same bounds as given.
	 */
	public FileRange getFirstRangeIn(SourceMap sourceMap, FileRange bounds, CoreExpr expr) {
		Option<Set<SourceExpr>> candidateSet = nonNull(this.coreExprToSourceExpr.get(expr));
		candidateSet = orSameSourceExpr(expr, candidateSet);
		return getFirstRangeIn(sourceMap, bounds, candidateSet);
	}

	private FileRange getFirstRangeIn(SourceMap sourceMap, FileRange bounds, final Option<Set<SourceExpr>> candidateSet) {
		FileRange result = bounds;
		for(final SourceExpr node : candidateSet.orSome(EMPTY_SOURCE_EXPR_SET)) {
			for(final FileRange candidate : sourceMap.get(nonNull(node))) {
				if(!candidate.isSubrange(bounds))
					continue;
				if(result == bounds || candidate.getStart().before(result.getStart())) {
					result = candidate;
				}
			}
		}
		return result;
	}

	/**
	 * Find the file range of the last occurrence of the given method's signature within the given bounds.
	 * 
	 * If there's no match, returns the same bounds as given.
	 */
	public FileRange getLastSignatureRangeIn(SourceMap sourceMap, FileRange bounds, Method method) {
		return getLastRangeIn(sourceMap, bounds, nonNull(this.methodSignatureToSourceExpr.get(method)));
	}

	/**
	 * Find the file range of the last occurrence of the given method's body within the given bounds.
	 * 
	 * If there's no match, returns the same bounds as given.
	 */
	public FileRange getLastBodyRangeIn(SourceMap sourceMap, FileRange bounds, Method method) {
		return getLastRangeIn(sourceMap, bounds, nonNull(this.methodBodyToSourceExpr.get(method)));
	}

	/**
	 * Find the file range of the last occurrence of the given method parameter within the given bounds.
	 * 
	 * If there's no match, returns the same bounds as given.
	 */
	public FileRange getLastRangeIn(SourceMap sourceMap, FileRange bounds, MethodParamDecl methodParamDecl) {
		return getLastRangeIn(sourceMap, bounds, nonNull(this.paramDeclToSourceExpr.get(methodParamDecl)));
	}

	/**
	 * Find the file range of the last occurrence of the given core expression within the given bounds.
	 * 
	 * If there's no match, returns the same bounds as given.
	 */
	public FileRange getLastRangeIn(SourceMap sourceMap, FileRange bounds, CoreExpr expr) {
		Option<Set<SourceExpr>> candidateSet = nonNull(this.coreExprToSourceExpr.get(expr));
		candidateSet = orSameSourceExpr(expr, candidateSet);
		return getLastRangeIn(sourceMap, bounds, candidateSet);
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


	private FileRange getLastRangeIn(SourceMap sourceMap, FileRange bounds, final Option<Set<SourceExpr>> candidateSet) {
		FileRange result = bounds;
		for(@SuppressWarnings("null") @NonNull final SourceExpr node : candidateSet.orSome(EMPTY_SOURCE_EXPR_SET)) {
			result = getLastRangeIn(sourceMap, bounds, result, node);
		}
		return result;
	}

	public FileRange getLastRangeIn(SourceMap sourceMap, FileRange bounds,
			FileRange result, final SourceExpr node) {
		for(@SuppressWarnings("null") @NonNull final FileRange candidate : sourceMap.get(node)) {
			if(!candidate.isSubrange(bounds))
				continue;
			if(result == bounds || !candidate.getStart().before(result.getStart())) {
				result = candidate;
			}
		}
		return result;
	}

	public DesugarMap insert(CoreExpr expr, SourceExpr sourceExpr) {
		if(expr.equals(sourceExpr))
			return this; // Don't bother
		final TreeMap<CoreExpr, Set<SourceExpr>> newSourceExprMap = nonNull(this.coreExprToSourceExpr.set(expr, this.coreExprToSourceExpr.get(expr).orSome(EMPTY_SOURCE_EXPR_SET).insert(sourceExpr)));
		return new DesugarMap(newSourceExprMap, this.methodSignatureToSourceExpr, this.methodBodyToSourceExpr, this.paramDeclToSourceExpr);
	}

	public DesugarMap insert(Method expr, SourceExpr signatureSourceExpr, SourceExpr bodySourceExpr) {
		final TreeMap<Method, Set<SourceExpr>> sig = nonNull(this.methodSignatureToSourceExpr.set(expr, this.methodSignatureToSourceExpr.get(expr).orSome(EMPTY_SOURCE_EXPR_SET).insert(signatureSourceExpr)));
		final TreeMap<Method, Set<SourceExpr>> body = nonNull(this.methodBodyToSourceExpr.set(expr, this.methodBodyToSourceExpr.get(expr).orSome(EMPTY_SOURCE_EXPR_SET).insert(bodySourceExpr)));
		return new DesugarMap(this.coreExprToSourceExpr, sig, body, this.paramDeclToSourceExpr);
	}

	public DesugarMap insert(MethodParamDecl methodParamDecl, SourceExpr sourceExpr) {
		final TreeMap<MethodParamDecl, Set<SourceExpr>> newParamDeclToSourceExpr = nonNull(this.paramDeclToSourceExpr.set(methodParamDecl, this.paramDeclToSourceExpr.get(methodParamDecl).orSome(EMPTY_SOURCE_EXPR_SET).insert(sourceExpr)));
		return new DesugarMap(this.coreExprToSourceExpr, this.methodSignatureToSourceExpr, this.methodBodyToSourceExpr, newParamDeclToSourceExpr);
	}

	public TreeMap<CoreExpr, fj.data.Set<SourceExpr>> getCoreExprToSourceExpr() {
		return this.coreExprToSourceExpr;
	}

	public TreeMap<MethodParamDecl, fj.data.Set<SourceExpr>> getParamDeclToSourceExpr() {
		return this.paramDeclToSourceExpr;
	}

	public TreeMap<Method, fj.data.Set<SourceExpr>> getMethodSignatureToSourceExpr() {
		return this.methodSignatureToSourceExpr;
	}

	public TreeMap<Method, fj.data.Set<SourceExpr>> getMethodBodyToSourceExpr() {
		return this.methodBodyToSourceExpr;
	}

	@SuppressWarnings("null")
	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("DesugarMap:\n");
		for(final P2<CoreExpr, Set<SourceExpr>> p : this.coreExprToSourceExpr) {
			for(final SourceExpr sourceExpr : p._2()) {
				sb.append("    ");
				sb.append(sourceExpr.toString());
				sb.append(" --> ");
				sb.append(p._1().toSource());
				sb.append('\n');
			}
		}
		for(final P2<MethodParamDecl, Set<SourceExpr>> p : this.paramDeclToSourceExpr) {
			for(final SourceExpr sourceExpr : p._2()) {
				sb.append("    ");
				sb.append(sourceExpr.toString());
				sb.append(" --> ");
				sb.append(p._1().toString());
				sb.append('\n');
			}
		}
		for(final P2<Method, Set<SourceExpr>> p : this.methodSignatureToSourceExpr) {
			for(final SourceExpr sourceExpr : p._2()) {
				sb.append("    ");
				sb.append(sourceExpr.toString());
				sb.append(" --> ");
				sb.append(p._1().toString());
				sb.append('\n');
			}
		}
		for(final P2<Method, Set<SourceExpr>> p : this.methodBodyToSourceExpr) {
			for(final SourceExpr sourceExpr : p._2()) {
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