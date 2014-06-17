package banjo.analysis;

import static banjo.parser.util.Check.nonNull;

import java.net.URI;

import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.BadCoreExpr;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.core.Extend;
import banjo.dom.core.Inspect;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.Method;
import banjo.dom.core.MethodFormalArgument;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.token.BadIdentifier;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.parser.util.FileRange;
import banjo.parser.util.SourceFileRange;
import fj.Ord;
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
	static final boolean debug = true;

	/**
	 * Analyse an AST without any source file information.
	 */
	public Analysis analyse(CoreExpr root) {
		return new Analysis().analyse(root);
	}
	public Analysis analyseMethod(Method method) {
		return new Analysis().analyseMethod(method);
	}
	public Analysis analyse(URI source, CoreExpr root, SourceFileRange wholeFileRange) {
		return new Analysis().analyse(root);
	}

	public static class Analysis {
		private static final Ord<FileRange> FILERANGE_ORD = Ord.<FileRange>comparableOrd();
		//static final Set<Identifier> EMPTY_FREE_LIST = nonNull(Set.empty(ExprOrd.<Identifier>exprOrd()));
		static final Ord<MethodFormalArgument> methodParamOrd = nonNull(Ord.<MethodFormalArgument>comparableOrd());
		static final Ord<Method> methodNodeRefOrd = nonNull(Ord.<Method>comparableOrd());
		static final Ord<Key> exprRefOrd = nonNull(Key.ORD);

		@SuppressWarnings("null") @NonNull
		static final Set<MethodFormalArgument> EMPTY_METHOD_PARAM_REF_SET = Set.empty(methodParamOrd);
		@SuppressWarnings("null") @NonNull
		static final Set<Key> EMPTY_KEY_SET = Set.<Key>empty(exprRefOrd);
		@SuppressWarnings("null") @NonNull
		static final List<CoreExpr> EMPTY_EXPR_LIST = List.<CoreExpr>nil();
		@SuppressWarnings("null") @NonNull
		static final TreeMap<MethodFormalArgument, Set<Key>> EMPTY_REFS_MAP = TreeMap.<MethodFormalArgument, Set<Key>>empty(methodParamOrd);
		@SuppressWarnings("null") @NonNull
		static final TreeMap<CoreExpr, Analysis> EMPTY_EXPR_ANALYSIS_CACHE = TreeMap.<CoreExpr, Analysis>empty(CoreExpr.ORD);

		/** Free variables in the analysed expression; theses are variables that are referenced but not defined */
		final Set<Key> free; // expressions that are a free variable
		final TreeMap<MethodFormalArgument, Set<Key>> refs; // def -> references
		final TreeMap<CoreExpr, Analysis> exprAnalysisCache; // exprRef -> analysis
		final Set<MethodFormalArgument> shadowingDefs; // Variables that shadow an outer variable of the same name
		final Set<MethodFormalArgument> unusedDefs; // Variables defined but not used
		final Set<Key> selfNames; // Method "self" parameter names

		// TODO: Types!
		// Types for methods are not checked / calculated until a method is in the call graph of the application
		// How do we know what's in the call graph of the application, though ?  How do we identify the "application" versus
		// modules?  Also how could we identify test cases, which could also be considered roots?

		// Perhaps types can be calculated in a bottom-up fashion much like defs/refs.  Each expression can be
		// transformed into an expression that manipulates types from the "type environment" - a version of
		// the environment that maps names to types.  When a variable is bound to a type we can substitute the type
		// for the variable in the current type environment.  This way objects that are defined and used locally will
		// be internally resolved.  References to external modules are substituted with their AST so this analysis can
		// be done over the root file of the project (build.banjo) to fully type the whole application.

		// TODO: Caching - if the analysis of subexpressions can be cached it might be a win.  This means the subexpressions
		// should be analysed without a parent environment and then linked/merged as we move back up the tree.  So variable
		// refs are recorded as free in the subexpression, then free variables are switched to refs as we find the defs and ascend the
		// the expression tree.

		public Analysis() {
			this(EMPTY_KEY_SET, EMPTY_REFS_MAP, EMPTY_EXPR_ANALYSIS_CACHE, EMPTY_METHOD_PARAM_REF_SET, EMPTY_METHOD_PARAM_REF_SET, EMPTY_KEY_SET);
		}

		public Analysis(Set<Key> free,
				TreeMap<MethodFormalArgument, Set<Key>> refs,
				TreeMap<CoreExpr, Analysis> exprAnalysisCache,
				Set<MethodFormalArgument> shadowingDefs,
				Set<MethodFormalArgument> unusedDefs,
				Set<Key> selfNames) {
			super();
			this.free = free;
			this.refs = refs;
			this.exprAnalysisCache = exprAnalysisCache;
			this.shadowingDefs = shadowingDefs;
			this.unusedDefs = unusedDefs;
			this.selfNames = selfNames;
		}

		private static Analysis withFree(Set<Key> free) {
			return new Analysis(free, EMPTY_REFS_MAP, EMPTY_EXPR_ANALYSIS_CACHE, EMPTY_METHOD_PARAM_REF_SET, EMPTY_METHOD_PARAM_REF_SET, EMPTY_KEY_SET);
		}

		public Analysis analyse(URI source, CoreExpr ast, FileRange wholeFileRange) {
			return analyse(ast);
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
			final Set<Key> newFree = this.free.union(other.free);
			final TreeMap<MethodFormalArgument, Set<Key>> newRefs = union(this.refs, other.refs);
			final TreeMap<CoreExpr, Analysis> newCache = putAll(this.exprAnalysisCache, other.exprAnalysisCache);
			final Set<MethodFormalArgument> newShadowingDefs = this.shadowingDefs.union(other.shadowingDefs);
			final Set<MethodFormalArgument> newUnusedDefs = this.unusedDefs.union(other.unusedDefs);
			final Set<Key> newSelfNames = this.selfNames.union(other.selfNames);
			return new Analysis(newFree,newRefs,newCache,newShadowingDefs,newUnusedDefs,newSelfNames);
		}

		private Analysis analyse(final CoreExpr nodeRef) {
			final Analysis cached = this.exprAnalysisCache.get(nodeRef).toNull();
			if(cached != null)
				return union(cached);

			final Analysis result = nonNull(nodeRef.acceptVisitor(new CoreExprVisitor<Analysis>() {
				@Override
				public Analysis inspect(Inspect n) {
					return analyse(n.getTarget());
				}

				@Override
				public Analysis extend(Extend n) {
					return analyse(n.getBase()).analyse(n.getExtension());
				}

				@Override
				public Analysis call(Call n) {
					Analysis a = analyse(n.getObject());
					for(final CoreExpr arg : n.getArguments()) {
						a = a.union(a.subAnalysis().analyse(nonNull(arg)));
					}
					return a;
				}

				@Override
				public Analysis listLiteral(ListLiteral n) {
					Analysis a = identifier(Identifier.EMPTY_LIST);
					for(final CoreExpr elt : n.getElements()) {
						a = a.analyse(nonNull(elt));
					}
					return a;
				}

				@Override
				public Analysis identifier(Identifier n) {
					//System.out.println("Ref: "+n);
					@NonNull @SuppressWarnings("null")
					final Set<Key> newFree = EMPTY_KEY_SET.insert(n);
					return withFree(newFree);
				}

				@Override
				public Analysis objectLiteral(ObjectLiteral n) {
					Analysis a = Analysis.this;
					for(final Method m : n.getMethods()) {
						a = a.analyseMethod(nonNull(m));
					}
					return a;
				}

				@Override
				public Analysis stringLiteral(StringLiteral stringLiteral) {
					return Analysis.this;
					// Strings are made from the empty string plus numbers (which are made from zero)
					//@NonNull @SuppressWarnings("null")
					//final Set<CoreExpr> newFree = EMPTY_EXPR_REF_SET.insert(new CoreExpr(nodeRef, Identifier.EMPTY_STRING)).insert(new CoreExpr(nodeRef, Identifier.ZERO));
					//return withFree(newFree);
				}

				@Override
				public Analysis numberLiteral(NumberLiteral numberLiteral) {
					return Analysis.this;
					// Numbers are made from zero and one
					//@NonNull @SuppressWarnings("null")
					//final Set<CoreExpr> newFree = EMPTY_EXPR_REF_SET.insert(new CoreExpr(nodeRef, Identifier.ZERO));
					//return withFree(newFree);
				}

				@Override
				public Analysis operator(OperatorRef operatorRef) {
					return Analysis.this;
				}

				@Override
				@Nullable
				public Analysis badExpr(BadCoreExpr badExpr) {
					return Analysis.this;
				}

				@Override
				@Nullable
				public Analysis badIdentifier(BadIdentifier badIdentifier) {
					@NonNull @SuppressWarnings("null")
					final Set<Key> newFree = EMPTY_KEY_SET.insert(badIdentifier);
					return withFree(newFree);
				}
			}));

			// Add to the cache
			return new Analysis(result.free, result.refs, result.exprAnalysisCache.set(nodeRef, result), result.shadowingDefs, result.unusedDefs, result.selfNames);
		}

		/**
		 * Analyse a method.
		 * 
		 * The method body, parameter assertions, and metho guarantee are analysed separately and then free variables
		 * are "linked" afterwards.  This allows us to cache the analysis of sub-expressions since that analysis
		 * doesn't depend on the environment.
		 */
		protected Analysis analyseMethod(CoreExpr objectNodeRef, Method m) {
			return analyseMethod(m);
		}

		/**
		 * Analyse a method.
		 * 
		 * The method body, parameter assertions, and metho guarantee are analysed separately and then free variables
		 * are "linked" afterwards.  This allows us to cache the analysis of sub-expressions since that analysis
		 * doesn't depend on the environment.
		 */
		protected Analysis analyseMethod(Method m) {
			System.out.println("Analyse method: "+m);
			TreeMap<String, MethodFormalArgument> newDefs = nonNull(TreeMap.<String, MethodFormalArgument>empty(Ord.stringOrd));
			for(final MethodFormalArgument param : m.getArguments()) {
				final String k = param.getName().getKeyString();
				newDefs = newDefs.set(k, param);
			}
			if(m.hasSelfName()) {
				final String k = m.getSelfName().getKeyString();
				newDefs = nonNull(newDefs.set(k, new MethodFormalArgument(m.getSelfName())));
			}
			Analysis a = subAnalysis();
			for(final MethodFormalArgument param : m.getArguments()) {
				if(param.hasAssertion()) {
					a = a.analyse(param.getAssertion());
				}
			}
			if(m.hasGuarantee()) {
				a = a.analyse(m.getGuarantee());
			}

			a = a.analyse(m.getBody());

			return union(a.link(newDefs));
		}

		/**
		 * Apply new definitions to this analysis such that all matching free references are changed into
		 * bound references.
		 */
		private Analysis link(TreeMap<String, MethodFormalArgument> newDefs) {
			// Check for defs in this.refs which are being bound again in an outer context - this means
			// the inner def "shadows" this outer def we just found
			Set<MethodFormalArgument> newShadowingDefs = this.shadowingDefs;
			for(final P2<MethodFormalArgument, ?> p : this.refs) {
				final MethodFormalArgument def = p._1();
				if(newDefs.contains(def.getName().getKeyString()))
					newShadowingDefs = nonNull(newShadowingDefs.insert(def));
			}

			// Move from free to refs as we "link" the definitions
			Set<Key> newFree = this.free;
			TreeMap<MethodFormalArgument, Set<Key>> newRefs = this.refs;
			for(final Key refExpr : this.free) {
				final String k = refExpr.toSource();
				final MethodFormalArgument defNodeRef = newDefs.get(k).toNull();
				if(defNodeRef != null) {
					System.out.println("Bound: "+refExpr);
					newFree = nonNull(newFree.delete(refExpr));
					newRefs = nonNull(newRefs.set(defNodeRef, newRefs.get(defNodeRef).orSome(EMPTY_KEY_SET).insert(refExpr)));
				} else {
					System.out.println("Not bound: "+refExpr);
				}
			}

			// Check for defs in newDefs that were never bound to anything
			Set<MethodFormalArgument> newUnusedDefs = this.unusedDefs;
			for(final P2<String, MethodFormalArgument> p : newDefs) {
				final MethodFormalArgument def = p._2();
				if(!newRefs.contains(def)) {
					System.out.println("Unused: "+def.getName());
					newUnusedDefs = nonNull(newUnusedDefs.insert(def));
				}
			}
			return new Analysis(newFree, newRefs, this.exprAnalysisCache, newShadowingDefs, newUnusedDefs, this.selfNames);
		}

		private Analysis subAnalysis() {
			return new Analysis(EMPTY_KEY_SET, EMPTY_REFS_MAP, this.exprAnalysisCache, EMPTY_METHOD_PARAM_REF_SET, EMPTY_METHOD_PARAM_REF_SET, EMPTY_KEY_SET);
		}

		public Set<Key> getFree() {
			return this.free;
		}

		public TreeMap<MethodFormalArgument, Set<Key>> getRefs() {
			return this.refs;
		}

		public Set<MethodFormalArgument> getShadowingDefs() {
			return this.shadowingDefs;
		}

		public Set<MethodFormalArgument> getUnusedDefs() {
			return this.unusedDefs;
		}

		public Set<Key> getSelfNames() {
			return this.selfNames;
		}


	}
}
