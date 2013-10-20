package banjo.analysis;

import static banjo.parser.util.Check.nonNull;

import java.net.URI;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.Extend;
import banjo.dom.core.Inspect;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.Method;
import banjo.dom.core.MethodParamDecl;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.parser.util.DesugarMap;
import banjo.parser.util.FileRange;
import banjo.parser.util.SourceMap;
import fj.F;
import fj.Ord;
import fj.Ordering;
import fj.P;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import fj.data.Set;
import fj.data.TreeMap;

/**
 * Analyse a Banjo AST to:
 * 
 * <ul>
 * <li>Report free variables</li>
 * <li>Report unused definitions</li>
 * <li>Determine the def / ref chain of variables</li>
 * </ul>
 * 
 * @author Dobes
 *
 */
public class DefRefAnalyser {

	public Analysis analyse(URI source, CoreExpr root, FileRange wholeFileRange) {
		return new Analysis().analyse(new ExprRef(new FileRef(source, wholeFileRange), root));
	}

	static interface NodeRef {
		/** Get the file range covered by this node */
		P2<FileRange, TreeMap<NodeRef, FileRange>> cacheSourceFileRange(DesugarMap dsMap, SourceMap sourceMap, TreeMap<NodeRef, FileRange> cache);

		/** True if this refers to a node in the given file */
		boolean isInFile(URI fileUri);
	}

	static int compare(NodeRef a, NodeRef b) {
		final int cmp = a.getClass().getName().compareTo(b.getClass().getName());
		if(cmp != 0) return cmp;
		if(a instanceof ExprRef)
			return ((ExprRef)a).compareTo((ExprRef)b);
		if(a instanceof MethodNodeRef)
			return ((MethodNodeRef)a).compareTo((MethodNodeRef)b);
		if(a instanceof MethodParamRef)
			return ((MethodParamRef)a).compareTo((MethodParamRef)b);
		if(a instanceof FileRef)
			return ((FileRef)a).compareTo((FileRef)b);
		throw new Error("Not implemented: DefRefAnalyser.compare of "+a.getClass().getName());
	}

	static class MethodNodeRef implements Comparable<MethodNodeRef>, NodeRef {
		private final ExprRef objectNodeRef;
		private final Method method;
		public MethodNodeRef(ExprRef objectNodeRef, Method method) {
			super();
			this.objectNodeRef = objectNodeRef;
			this.method = method;
		}

		@Override
		public int compareTo(MethodNodeRef o) {
			if(o == this) return 0;
			int cmp = this.objectNodeRef.compareTo(o.objectNodeRef);
			if(cmp == 0) cmp = this.method.compareTo(o.method);
			return cmp;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + this.objectNodeRef.hashCode();
			result = prime * result + this.method.hashCode();
			return result;
		}

		@Override
		public boolean equals(@Nullable Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (!(obj instanceof MethodNodeRef))
				return false;
			final MethodNodeRef other = (MethodNodeRef) obj;
			if (!this.method.equals(other.method))
				return false;
			if (!this.objectNodeRef.equals(other.objectNodeRef))
				return false;
			return true;
		}

		public NodeRef getObjectNodeRef() {
			return this.objectNodeRef;
		}

		public Method getMethod() {
			return this.method;
		}

		@Override
		public P2<FileRange, TreeMap<NodeRef, FileRange>> cacheSourceFileRange(DesugarMap dsMap, SourceMap sourceMap, TreeMap<NodeRef, FileRange> cache) {
			final FileRange cachedRange = cache.get(this).toNull();
			if(cachedRange != null)
				return nonNull(P.p(cachedRange, cache));
			final P2<FileRange, TreeMap<NodeRef, FileRange>> p = this.objectNodeRef.cacheSourceFileRange(dsMap, sourceMap, cache);
			final TreeMap<NodeRef, FileRange> cache2 = p._2();
			final FileRange parentRange = p._1();
			// Use body range here since the body children will ask for this generically, the parameters
			// are going to look for the signature range themselves.
			final FileRange r = dsMap.getLastBodyRangeIn(sourceMap, nonNull(parentRange), this.method);
			final TreeMap<NodeRef, FileRange> cache3 = cache2.set(this, r);
			//System.out.println("Method "+this.method+" range is {"+r+"} in {"+parentRange+"}");
			return nonNull(P.p(r, cache3));
		}

		@Override
		public boolean isInFile(URI fileUri) {
			return this.objectNodeRef.isInFile(fileUri);
		}

	}

	static class MethodParamRef implements NodeRef, Comparable<MethodParamRef> {
		private final MethodNodeRef methodRef;
		private final MethodParamDecl paramDecl;
		public MethodParamRef(MethodNodeRef methodRef, MethodParamDecl paramDecl) {
			super();
			this.methodRef = methodRef;
			this.paramDecl = paramDecl;
		}

		@Override
		public P2<FileRange, TreeMap<NodeRef, FileRange>> cacheSourceFileRange(DesugarMap dsMap, SourceMap sourceMap, TreeMap<NodeRef, FileRange> cache) {
			final FileRange cachedRange = cache.get(this).toNull();
			if(cachedRange != null)
				return nonNull(P.p(cachedRange, cache));
			final P2<FileRange, TreeMap<NodeRef, FileRange>> p = this.methodRef.objectNodeRef.cacheSourceFileRange(dsMap, sourceMap, cache);
			final TreeMap<NodeRef, FileRange> cache2 = p._2();
			final FileRange parentParentRange = nonNull(p._1());
			final FileRange parentRange = dsMap.getFirstSignatureRangeIn(sourceMap, parentParentRange, this.methodRef.getMethod());
			final FileRange r = dsMap.getFirstRangeIn(sourceMap, nonNull(parentRange), this.paramDecl);
			final TreeMap<NodeRef, FileRange> cache3 = cache2.set(this, r);
			//System.out.println("Param `"+this.paramDecl.getName()+"` in '"+this.methodRef.method+"' range {"+r+"} in {"+parentRange+"} in {"+parentParentRange+"}");
			return nonNull(P.p(r, cache3));
		}

		public MethodNodeRef getMethodRef() {
			return this.methodRef;
		}

		public MethodParamDecl getParamDecl() {
			return this.paramDecl;
		}

		public String getName() {
			return this.paramDecl.getName().getKeyString();
		}

		@Override
		public int compareTo(MethodParamRef o) {
			if(this == o) return 0;
			int cmp = this.methodRef.compareTo(o.methodRef);
			if(cmp == 0) cmp = this.paramDecl.compareTo(o.paramDecl);
			return cmp;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + this.methodRef.hashCode();
			result = prime * result + this.paramDecl.hashCode();
			return result;
		}

		@Override
		public boolean equals(@Nullable Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (!(obj instanceof MethodParamRef))
				return false;
			final MethodParamRef other = (MethodParamRef) obj;
			if (!this.methodRef.equals(other.methodRef))
				return false;
			if (!this.paramDecl.equals(other.paramDecl))
				return false;
			return true;
		}

		@Override
		public boolean isInFile(URI fileUri) {
			return this.methodRef.isInFile(fileUri);
		}
	}

	static class ExprRef implements NodeRef, Comparable<ExprRef> {
		private final NodeRef parent;
		private final CoreExpr node;
		public ExprRef(NodeRef parent, CoreExpr node) {
			super();
			this.parent = parent;
			this.node = node;
		}
		public NodeRef getParent() {
			return this.parent;
		}

		public CoreExpr getNode() {
			return this.node;
		}

		@Override
		public P2<FileRange, TreeMap<NodeRef, FileRange>> cacheSourceFileRange(DesugarMap dsMap, SourceMap sourceMap, TreeMap<NodeRef, FileRange> cache) {
			final FileRange cachedRange = cache.get(this).toNull();
			if(cachedRange != null)
				return nonNull(P.p(cachedRange, cache));
			final P2<FileRange, TreeMap<NodeRef, FileRange>> p = this.parent.cacheSourceFileRange(dsMap, sourceMap, cache);
			final TreeMap<NodeRef, FileRange> cache2 = p._2();
			final FileRange parentRange = p._1();
			final FileRange r = dsMap.getLastRangeIn(sourceMap, nonNull(parentRange), this.node);
			final TreeMap<NodeRef, FileRange> cache3 = cache2.set(this, r);
			//System.out.println("Expr "+this.node.getClass().getSimpleName()+"(\""+this.node+"\") range {"+r+"} in {"+parentRange+"}");
			return nonNull(P.p(r, cache3));
		}


		@Override
		public int compareTo(ExprRef other) {
			int cmp = compare(this.parent, other.getParent());
			if(cmp == 0) cmp = this.node.compareTo(other.getNode());
			return cmp;
		}
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + this.node.hashCode();
			result = prime * result	+ this.parent.hashCode();
			return result;
		}
		@Override
		public boolean equals(@Nullable Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (!(obj instanceof ExprRef))
				return false;
			final ExprRef other = (ExprRef) obj;
			if (!this.node.equals(other.node))
				return false;
			if (!this.parent.equals(other.parent))
				return false;
			return true;
		}
		@Override
		public boolean isInFile(URI fileUri) {
			return this.parent.isInFile(fileUri);
		}
	}

	static class FileRef implements NodeRef, Comparable<FileRef> {
		private final URI source;
		private final FileRange wholeFileRange;

		public FileRef(URI source, FileRange wholeFileRange) {
			this.source = source;
			this.wholeFileRange = wholeFileRange;
		}
		public URI getSource() {
			return this.source;
		}

		@Override
		public P2<FileRange, TreeMap<NodeRef, FileRange>> cacheSourceFileRange(DesugarMap dsMap, SourceMap sourceMap, TreeMap<NodeRef, FileRange> cache) {
			return nonNull(P.p(this.wholeFileRange, cache));
		}

		@Override
		public int compareTo(FileRef o) {
			return this.source.compareTo(o.getSource());
		}
		@Override
		public int hashCode() {
			return this.source.hashCode();
		}
		@Override
		public boolean equals(@Nullable Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (!(obj instanceof FileRef))
				return false;
			final FileRef other = (FileRef) obj;
			return this.source.equals(other.source);
		}
		@Override
		public boolean isInFile(URI fileUri) {
			return this.source.equals(fileUri);
		}


	}

	static class Context {

	}
	public static class Analysis {
		private static final Ord<FileRange> FILERANGE_ORD = Ord.<FileRange>comparableOrd();
		//static final Set<Identifier> EMPTY_FREE_LIST = nonNull(Set.empty(ExprOrd.<Identifier>exprOrd()));
		static final Ord<MethodParamRef> defNodeOrd = nonNull(Ord.<MethodParamRef>comparableOrd());
		static final Ord<MethodNodeRef> methodNodeRefOrd = nonNull(Ord.<MethodNodeRef>comparableOrd());
		static final Ord<ExprRef> exprRefOrd = nonNull(Ord.<ExprRef>comparableOrd());
		static final Ord<NodeRef> nodeRefOrd = nonNull(Ord.ord(
				new F<NodeRef, F<NodeRef, Ordering>>() {
					@Override
					public F<NodeRef, Ordering> f(final NodeRef a1) {
						return new F<NodeRef, Ordering>() {
							@Override
							public Ordering f(final NodeRef a2) {
								final int x = compare(a1, a2);
								return nonNull(x < 0 ? Ordering.LT : x == 0 ? Ordering.EQ : Ordering.GT);
							}
						};
					}
				}));
		@SuppressWarnings("null") @NonNull
		static final Set<MethodParamRef> EMPTY_METHOD_PARAM_REF_SET = Set.empty(defNodeOrd);
		@SuppressWarnings("null") @NonNull
		static final Set<ExprRef> EMPTY_EXPR_REF_SET = Set.<ExprRef>empty(exprRefOrd);
		@SuppressWarnings("null") @NonNull
		static final List<NodeRef> EMPTY_NODE_REF_LIST = List.<NodeRef>nil();
		@SuppressWarnings("null") @NonNull
		static final Set<NodeRef> EMPTY_NODE_REF_SET = Set.<NodeRef>empty(nodeRefOrd);
		@SuppressWarnings("null") @NonNull
		static final List<CoreExpr> EMPTY_EXPR_LIST = List.<CoreExpr>nil();
		@SuppressWarnings("null") @NonNull
		static final TreeMap<MethodParamRef, Set<ExprRef>> EMPTY_REFS_MAP = TreeMap.<MethodParamRef, Set<ExprRef>>empty(defNodeOrd);
		@SuppressWarnings("null") @NonNull
		static final TreeMap<ExprRef, Analysis> EMPTY_EXPR_ANALYSIS_CACHE = TreeMap.<ExprRef, Analysis>empty(exprRefOrd);

		/** Free variables in the analysed expression; theses are variables that are referenced but not defined */
		final Set<ExprRef> free; // name -> free reference
		final TreeMap<MethodParamRef, Set<ExprRef>> refs; // def -> references
		final TreeMap<ExprRef, Analysis> exprAnalysisCache; // exprRef -> analysis
		final Set<MethodParamRef> shadowingDefs; // Variables that shadow an outer variable of the same name
		final Set<MethodParamRef> unusedDefs; // Variables defined but not used

		// TODO: Types!
		// Types for methods are not checked / calculated until a method is in the call graph of the application
		// How do we know what's in the call graph of the application, though ?  How do we identify the "application" versus
		// modules?  Also how could we identify test cases, which could also be considered roots?

		// TODO: Caching - if the analysis of subexpressions can be cached it might be a win.  This means the subexpressions
		// should be analysed without a parent environment and then linked/merged as we move back up the tree.  So variable
		// refs are recorded as free in the subexpression, then free variables are switched to refs as we find the defs and ascend the
		// the expression tree.

		public Analysis() {
			this(EMPTY_EXPR_REF_SET, EMPTY_REFS_MAP, EMPTY_EXPR_ANALYSIS_CACHE, EMPTY_METHOD_PARAM_REF_SET, EMPTY_METHOD_PARAM_REF_SET);
		}

		public Analysis(Set<ExprRef> free,
				TreeMap<MethodParamRef, Set<ExprRef>> refs,
				TreeMap<ExprRef, Analysis> exprAnalysisCache,
				Set<MethodParamRef> shadowingDefs,
				Set<MethodParamRef> unusedDefs) {
			super();
			this.free = free;
			this.refs = refs;
			this.exprAnalysisCache = exprAnalysisCache;
			this.shadowingDefs = shadowingDefs;
			this.unusedDefs = unusedDefs;
		}

		private Analysis withFree(Set<ExprRef> free) {
			return new Analysis(free, EMPTY_REFS_MAP, EMPTY_EXPR_ANALYSIS_CACHE, EMPTY_METHOD_PARAM_REF_SET, EMPTY_METHOD_PARAM_REF_SET);
		}

		public Analysis analyse(URI source, CoreExpr ast, FileRange wholeFileRange) {
			return analyse(new ExprRef(new FileRef(source, wholeFileRange), ast));
		}

		static <K,T> TreeMap<K, Set<T>> union(TreeMap<K, Set<T>> defs1, TreeMap<K, Set<T>> defs2) {
			TreeMap<K, Set<T>> result = defs1;
			for(final P2<K, Set<T>> pair : defs2) {
				final Option<Set<T>> oldSet = result.get(pair._1());
				@SuppressWarnings("null") @NonNull
				final TreeMap<K, Set<T>> newResult = oldSet.isSome() ? result.set(pair._1(), oldSet.some().union(pair._2())) : result.set(pair._1(), pair._2());
				result = newResult;
			}
			return result;
		}

		static <K,V> TreeMap<K,V> putAll(TreeMap<K,V> a, TreeMap<K,V> b) {
			for(final P2<K,V> p : b) {
				a = nonNull(a.set(p._1(), p._2()));
			}
			return a;
		}

		/**
		 * Combine to Analyses such that the various sets are simply extended
		 * to include the values from each other.  The two analyses are assumed to be
		 * independent - that is, they have separate scopes.
		 */
		public Analysis union(Analysis other) {
			@SuppressWarnings("null") @NonNull
			final Set<ExprRef> newFree = this.free.union(other.free);
			final TreeMap<MethodParamRef, Set<ExprRef>> newRefs = union(this.refs, other.refs);
			final TreeMap<ExprRef, Analysis> newCache = putAll(this.exprAnalysisCache, other.exprAnalysisCache);
			@SuppressWarnings("null") @NonNull
			final Set<MethodParamRef> newShadowingDefs = this.shadowingDefs.union(other.shadowingDefs);
			@SuppressWarnings("null") @NonNull
			final Set<MethodParamRef> newUnusedDefs = this.unusedDefs.union(other.unusedDefs);
			return new Analysis(newFree,newRefs,newCache,newShadowingDefs,newUnusedDefs);
		}

		private Analysis analyse(final ExprRef nodeRef) {
			final Analysis cached = this.exprAnalysisCache.get(nodeRef).toNull();
			if(cached != null)
				return union(cached);

			final Analysis result = nonNull(nodeRef.getNode().acceptVisitor(new BaseCoreExprVisitor<Analysis>() {
				@Override
				public Analysis fallback(CoreExpr unsupported) {
					return Analysis.this;
				}

				@Override
				@Nullable
				public Analysis inspect(Inspect n) {
					return analyse(new ExprRef(nodeRef, n.getTarget()));
				}

				@Override
				@Nullable
				public Analysis extend(Extend n) {
					return analyse(new ExprRef(nodeRef, n.getBase())).analyse(new ExprRef(nodeRef, n.getExtension()));
				}

				@Override
				@Nullable
				public Analysis call(Call n) {
					Analysis a = analyse(new ExprRef(nodeRef, n.getObject()));
					for(final CoreExpr arg : n.getArguments()) {
						a = a.union(a.subAnalysis().analyse(new ExprRef(nodeRef, nonNull(arg))));
					}
					return a;
				}

				@Override
				@Nullable
				public Analysis listLiteral(ListLiteral n) {
					Analysis a = Analysis.this;
					for(final CoreExpr elt : n.getElements()) {
						a = a.analyse(new ExprRef(nodeRef, nonNull(elt)));
					}
					return a;
				}

				@Override
				@Nullable
				public Analysis identifier(Identifier n) {
					//System.out.println("Ref: "+n);
					return withFree(nonNull(EMPTY_EXPR_REF_SET.insert(nodeRef)));
				}

				@Override
				@Nullable
				public Analysis objectLiteral(ObjectLiteral n) {
					Analysis a = Analysis.this;
					for(final Method m : n.getMethods()) {
						a = a.analyseMethod(nodeRef, nonNull(m));
					}
					return a;
				}
			}));

			// Add to the cache
			return new Analysis(result.free, result.refs, nonNull(result.exprAnalysisCache.set(nodeRef, result)), result.shadowingDefs, result.unusedDefs);
		}

		/**
		 * Analyse a method.
		 * 
		 * The method body, parameter assertions, and metho guarantee are analysed separately and then free variables
		 * are "linked" afterwards.  This allows us to cache the analysis of sub-expressions since that analysis
		 * doesn't depend on the environment.
		 */
		protected Analysis analyseMethod(ExprRef objectNodeRef, Method m) {
			final MethodNodeRef methodRef = new MethodNodeRef(objectNodeRef, m);
			TreeMap<String, MethodParamRef> newDefs = nonNull(TreeMap.<String, MethodParamRef>empty(Ord.stringOrd));
			for(final MethodParamDecl param : m.getArgs()) {
				final String k = param.getName().getKeyString();
				final MethodParamRef defNode = new MethodParamRef(methodRef, param);
				newDefs = nonNull(newDefs.set(k, defNode));
			}
			if(m.hasSelfName()) {
				final String k = m.getSelfName().getKeyString();
				final MethodParamRef defNode = new MethodParamRef(methodRef, new MethodParamDecl(m.getSelfName()));
				newDefs = nonNull(newDefs.set(k, defNode));
			}
			Analysis a = subAnalysis();
			for(final MethodParamDecl param : m.getArgs()) {
				if(param.hasAssertion()) {
					a = a.analyse(new MethodParamRef(methodRef, param), param.getAssertion());
				}
			}
			if(m.hasGuarantee()) {
				a = a.analyse(methodRef, m.getGuarantee());
			}

			a = a.analyse(methodRef, m.getBody());

			return union(a.link(newDefs));
		}

		/**
		 * Apply new definitions to this analysis such that all matching free references are changed into
		 * bound references.
		 */
		private Analysis link(TreeMap<String, MethodParamRef> newDefs) {
			// Check for defs in this.refs which are being bound again in an outer context - this means
			// the inner def "shadows" this outer def we just found
			Set<MethodParamRef> newShadowingDefs = this.shadowingDefs;
			for(final P2<MethodParamRef, ?> p : this.refs) {
				final MethodParamRef def = p._1();
				if(newDefs.contains(def.getName()))
					newShadowingDefs = nonNull(newShadowingDefs.insert(def));
			}

			// Move from free to refs as we "link" the definitions
			Set<ExprRef> newFree = this.free;
			TreeMap<MethodParamRef, Set<ExprRef>> newRefs = this.refs;
			for(final ExprRef refNodeRef : this.free) {
				final String k = ((Key)refNodeRef.getNode()).getKeyString();
				final MethodParamRef defNodeRef = newDefs.get(k).toNull();
				if(defNodeRef != null) {
					//System.out.println("Bound: "+refNodeRef.getNode());
					newFree = nonNull(newFree.delete(refNodeRef));
					newRefs = nonNull(newRefs.set(defNodeRef, newRefs.get(defNodeRef).orSome(EMPTY_EXPR_REF_SET).insert(refNodeRef)));
				}
			}

			// Check for defs in newDefs that were never bound to anything
			Set<MethodParamRef> newUnusedDefs = this.unusedDefs;
			for(final P2<String, MethodParamRef> p : newDefs) {
				final MethodParamRef def = p._2();
				if(!newRefs.contains(def)) {
					//System.out.println("Unused: "+def.getName());
					newUnusedDefs = nonNull(newUnusedDefs.insert(def));
				}
			}
			return new Analysis(newFree, newRefs, this.exprAnalysisCache, newShadowingDefs, newUnusedDefs);
		}

		private Analysis subAnalysis() {
			return new Analysis(EMPTY_EXPR_REF_SET, EMPTY_REFS_MAP, this.exprAnalysisCache, EMPTY_METHOD_PARAM_REF_SET, EMPTY_METHOD_PARAM_REF_SET);
		}

		private Analysis analyse(NodeRef parentNodeRef, CoreExpr childExpr) {
			return analyse(new ExprRef(parentNodeRef, childExpr));
		}

		public Set<ExprRef> getFree() {
			return this.free;
		}

		public TreeMap<MethodParamRef, Set<ExprRef>> getRefs() {
			return this.refs;
		}

		public Set<MethodParamRef> getShadowingDefs() {
			return this.shadowingDefs;
		}

		public Set<MethodParamRef> getUnusedDefs() {
			return this.unusedDefs;
		}

		public SourceRangeAnalysis calculateSourceRanges(DesugarMap dsMap, SourceMap sourceMap) {
			@SuppressWarnings("null") @NonNull
			TreeMap<NodeRef, FileRange> cache = TreeMap.<NodeRef,FileRange>empty(nodeRefOrd);
			@SuppressWarnings("null") @NonNull
			TreeMap<FileRange, String> identifiers = TreeMap.<FileRange,String>empty(FILERANGE_ORD);
			@SuppressWarnings("null") @NonNull
			Set<FileRange> free = Set.empty(FILERANGE_ORD);
			for(final ExprRef freeVar : this.free) {
				final P2<FileRange, TreeMap<NodeRef, FileRange>> p = freeVar.cacheSourceFileRange(dsMap, sourceMap, cache);
				@SuppressWarnings("null") @NonNull
				final TreeMap<NodeRef, FileRange> newCache = p._2();
				cache = newCache;
				final FileRange range = p._1();
				@SuppressWarnings("null") @NonNull
				final Set<FileRange> newFree = free.insert(range);
				free = newFree;

				@SuppressWarnings("null") @NonNull
				final TreeMap<FileRange, String> newIdentifiers = identifiers.set(range, freeVar.getNode().toSource());
				identifiers = newIdentifiers;
			}
			@SuppressWarnings("null") @NonNull
			Set<FileRange> shadowingDefs = Set.empty(FILERANGE_ORD);
			for(final MethodParamRef paramRef : this.shadowingDefs) {
				final P2<FileRange, TreeMap<NodeRef, FileRange>> p = paramRef.cacheSourceFileRange(dsMap, sourceMap, cache);
				@SuppressWarnings("null") @NonNull
				final TreeMap<NodeRef, FileRange> newCache = p._2();
				cache = newCache;
				final FileRange range = p._1();
				@SuppressWarnings("null") @NonNull
				final Set<FileRange> newShadowingDefs = shadowingDefs.insert(range);
				shadowingDefs = newShadowingDefs;
				@SuppressWarnings("null") @NonNull
				final TreeMap<FileRange, String> newIdentifiers = identifiers.set(range, paramRef.getName());
				identifiers = newIdentifiers;
			}
			@SuppressWarnings("null") @NonNull
			Set<FileRange> unusedDefs = Set.empty(FILERANGE_ORD);
			for(final MethodParamRef paramRef : this.unusedDefs) {
				final P2<FileRange, TreeMap<NodeRef, FileRange>> p = paramRef.cacheSourceFileRange(dsMap, sourceMap, cache);
				@SuppressWarnings("null") @NonNull
				final TreeMap<NodeRef, FileRange> newCache = p._2();
				cache = newCache;
				final FileRange range = p._1();
				@SuppressWarnings("null") @NonNull
				final Set<FileRange> newUnusedDefs = unusedDefs.insert(range);
				unusedDefs = newUnusedDefs;
				@SuppressWarnings("null") @NonNull
				final TreeMap<FileRange, String> newIdentifiers = identifiers.set(range, paramRef.getName());
				identifiers = newIdentifiers;
			}
			@SuppressWarnings("null") @NonNull
			TreeMap<FileRange, FileRange> refs = TreeMap.empty(FILERANGE_ORD);
			for(final P2<MethodParamRef, Set<ExprRef>> pair : this.refs) {
				final MethodParamRef paramRef = pair._1();
				final P2<FileRange, TreeMap<NodeRef, FileRange>> dp = paramRef.cacheSourceFileRange(dsMap, sourceMap, cache);
				@SuppressWarnings("null") @NonNull
				final TreeMap<NodeRef, FileRange> newCache = dp._2();
				cache = newCache;
				final FileRange defRange = dp._1();
				for(final ExprRef ref : pair._2()) {
					final P2<FileRange, TreeMap<NodeRef, FileRange>> p = ref.cacheSourceFileRange(dsMap, sourceMap, cache);
					@SuppressWarnings("null") @NonNull
					final TreeMap<NodeRef, FileRange> newCache2 = p._2();
					cache = newCache2;
					final FileRange refRange = p._1();
					@SuppressWarnings("null") @NonNull
					final TreeMap<FileRange, FileRange> newRefs = refs.set(refRange, defRange);
					refs = newRefs;
					@SuppressWarnings("null") @NonNull
					final TreeMap<FileRange, String> newIdentifiers = identifiers.set(refRange, ref.getNode().toSource());
					identifiers = newIdentifiers;
				}
				@SuppressWarnings("null") @NonNull
				final TreeMap<FileRange, String> newIdentifiers = identifiers.set(defRange, paramRef.getName());
				identifiers = newIdentifiers;
			}
			return new SourceRangeAnalysis(free, refs, shadowingDefs, unusedDefs, identifiers);
		}


		//		protected Analysis withFree(Identifier n, NodeRef nodeRef) {
		//			final TreeMap<String, Set<NodeRef>> newFree = nonNull(this.free.set(n.getKeyString(), this.free.get(n.getKeyString()).orSome(EMPTY_NODE_SET).insert(nodeRef)));
		//			return new Analysis(newFree, this.defs, this.unusedDefs, this.refs);
		//		}
		//
		//		private Analysis withRef(Identifier id, NodeRef ref, DefNodeRef def) {
		//			final String k = id.getKeyString();
		//			final TreeMap<String, Set<DefNodeRef>> newDefs = nonNull(this.defs.set(k, this.defs.get(k).orSome(EMPTY_DEF_LOC_SET).insert(def)));
		//			final Set<DefNodeRef> newUnusedDefs = nonNull(this.unusedDefs.delete(def));
		//			final TreeMap<DefNodeRef, Set<NodeRef>> newRefs = nonNull(this.refs.set(def, this.refs.get(def).orSome(EMPTY_NODE_SET).insert(ref)));
		//			return new Analysis(this.free, newDefs, newUnusedDefs, newRefs);
		//		}

	}

	public static class SourceRangeAnalysis {
		final Set<FileRange> free;
		final TreeMap<FileRange, FileRange> refs;
		final Set<FileRange> shadowingDefs;
		final Set<FileRange> unusedDefs;
		private final TreeMap<FileRange, String> identifiers;

		public SourceRangeAnalysis(Set<FileRange> free,
				TreeMap<FileRange, FileRange> refs,
				Set<FileRange> shadowingDefs, Set<FileRange> unusedDefs,
				TreeMap<FileRange, String> identifiers) {
			super();
			this.free = free;
			this.refs = refs;
			this.shadowingDefs = shadowingDefs;
			this.unusedDefs = unusedDefs;
			this.identifiers = identifiers;
		}
		public Set<FileRange> getFree() {
			return this.free;
		}
		public TreeMap<FileRange, FileRange> getRefs() {
			return this.refs;
		}
		public Set<FileRange> getShadowingDefs() {
			return this.shadowingDefs;
		}
		public Set<FileRange> getUnusedDefs() {
			return this.unusedDefs;
		}
		public TreeMap<FileRange, String> getIdentifiers() {
			return this.identifiers;
		}

	}
}
