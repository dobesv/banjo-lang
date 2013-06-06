package banjo.analysis;

import static banjo.parser.util.Check.nonNull;

import java.io.IOException;
import java.util.Map;
import java.util.Set;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.BadExpr;
import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.core.ExprList;
import banjo.dom.core.FunArg;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.Let;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.Method;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.core.Projection;
import banjo.dom.core.SetLiteral;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.parser.BanjoScanner;
import banjo.parser.errors.UnexpectedIOExceptionError;
import banjo.parser.util.ParserReader;
import fj.Ord;
import fj.data.Option;
import fj.data.TreeMap;

public class DefRefScanner {

	private final BanjoScanner scanner = new BanjoScanner();

	public static class ScanningExprVisitor implements CoreExprVisitor<Void> {
		public static final LocalDefTypeCalculator localDefTypeCalculator = new LocalDefTypeCalculator(DefType.LOCAL_VALUE, DefType.LOCAL_CONST, DefType.LOCAL_FUNCTION);
		public static final LocalDefTypeCalculator fieldDefTypeCalculator = new LocalDefTypeCalculator(DefType.SELF_FIELD, DefType.SELF_CONST, DefType.SELF_METHOD);
		protected final DefRefVisitor visitor;
		final TreeMap<String, DefInfo> environment;
		final int parameterScopeDepth;
		final int objectDepth;
		final int letDepth;

		final int exprSourceOffset;

		public ScanningExprVisitor(DefRefVisitor visitor) {
			this(visitor, 0, 0, 0, 0, nonNull(TreeMap.<String,DefInfo>empty(Ord.stringOrd)));
		}

		public ScanningExprVisitor(DefRefVisitor visitor, int exprSourceOffset,
				int parameterScopeDepth, int objectDepth, int letDepth, fj.data.TreeMap<String,DefInfo> environment) {
			super();
			this.visitor = visitor;
			this.exprSourceOffset = exprSourceOffset;
			this.parameterScopeDepth = parameterScopeDepth;
			this.objectDepth = objectDepth;
			this.letDepth = letDepth;
			this.environment = environment;
		}

		ScanningExprVisitor descend(int newParentExprOffset, int newParameterScopeDepth, int newObjectDepth, int newLetDepth, fj.data.TreeMap<String,DefInfo> newEnvironment) {
			return new ScanningExprVisitor(this.visitor, newParentExprOffset, newParameterScopeDepth, newObjectDepth, newLetDepth, newEnvironment);
		}

		protected final void scan(CoreExpr e) {
			scan(e, this.exprSourceOffset + e.getOffsetInParent());
		}
		protected final void scan(CoreExpr e, int sourceOffset) {
			scan(e, sourceOffset, this.parameterScopeDepth, this.objectDepth, this.letDepth, this.environment);
		}
		protected void scan(CoreExpr e, int exprSourceOffset, int newParameterScopeDepth, int newObjectDepth, int newLetDepth, fj.data.TreeMap<String,DefInfo> newEnvironment) {
			e.acceptVisitor(this.descend(this.exprSourceOffset + e.getOffsetInParent(), newParameterScopeDepth, newObjectDepth, newLetDepth, newEnvironment));
		}

		@Override
		@Nullable
		public Void stringLiteral(StringLiteral stringLiteral) {
			return null;
		}

		@Override
		@Nullable
		public Void numberLiteral(NumberLiteral numberLiteral) {
			return null;
		}

		@Nullable
		public Void visitRef(Key key) {
			final Option<DefInfo> binding = this.environment.get(key.getKeyString());
			if(binding.isSome()) {
				this.visitor.visitRef(nonNull(binding.some()), this.exprSourceOffset, key);
			} else {
				this.visitor.visitUnresolved(this.exprSourceOffset, key);
			}
			return null;
		}

		@Override
		@Nullable
		public Void identifier(Identifier identifier) {
			return visitRef(identifier);
		}

		@Override
		@Nullable
		public Void operator(OperatorRef operatorRef) {
			// TODO - resolve operators to functions ?
			return null;
		}

		@Override
		@Nullable
		public Void call(Call call) {
			scan(call.getCallee());
			for(final CoreExpr arg : call.getArguments()) {
				scan(nonNull(arg));
			}
			return null;
		}

		@Override
		@Nullable
		public Void exprList(ExprList exprList) {
			TreeMap<String, DefInfo> newEnvironment = this.environment;
			final int newLetDepth = this.letDepth+1;
			for(final CoreExpr e : exprList.getElements()) {
				final TreeMap<String, DefInfo> tempEnvironment = nonNull(newEnvironment);
				newEnvironment = nonNull(e.acceptVisitor(new BaseCoreExprVisitor<TreeMap<String, DefInfo>>() {
					@Override
					@Nullable
					public TreeMap<String, DefInfo> let(Let let) {
						final Key name = let.getName();
						final int nameSourceOffset = ScanningExprVisitor.this.exprSourceOffset + e.getOffsetInParent() + name.getOffsetInParent();
						final DefType defType = letDefType(let);
						return def(name, nameSourceOffset, defType, newLetDepth, tempEnvironment);
					}

					@Override
					@Nullable
					public TreeMap<String, DefInfo> fallback(
							CoreExpr unsupported) {
						return tempEnvironment;
					}
				}));
			}
			for(final CoreExpr e : exprList.getElements()) {
				final TreeMap<String, DefInfo> tempEnvironment = nonNull(newEnvironment);
				e.acceptVisitor(new BaseCoreExprVisitor<Void>() {

					@Override
					@Nullable
					public Void let(Let n) {
						scan(n.getValue(), ScanningExprVisitor.this.exprSourceOffset + n.getOffsetInParent() + n.getValue().getOffsetInParent(), ScanningExprVisitor.this.parameterScopeDepth, ScanningExprVisitor.this.objectDepth, newLetDepth, tempEnvironment);
						return null;
					}

					@Override
					@Nullable
					public Void fallback(CoreExpr e) {
						scan(e, ScanningExprVisitor.this.exprSourceOffset + e.getOffsetInParent(), ScanningExprVisitor.this.parameterScopeDepth, ScanningExprVisitor.this.objectDepth, newLetDepth, tempEnvironment);
						return null;
					}
				});
			}
			return null;
		}

		private TreeMap<String, DefInfo> def(Key name, int nameSourceOffset, DefType defType, int scopeDepth, TreeMap<String, DefInfo> environment) {
			if(name.getSourceLength() == 0) return environment; // Ignore synthetic nodes
			final DefInfo def = new DefInfo(name, nameSourceOffset, defType, scopeDepth);
			this.visitor.visitDef(def);
			return nonNull(environment.set(name.getKeyString(), def));
		}

		private DefType letDefType(Let let) {
			final DefType defType = let.getValue().acceptVisitor(ScanningExprVisitor.localDefTypeCalculator);
			if(defType == null) throw new NullPointerException();
			return defType;
		}

		@Override
		@Nullable
		public Void projection(Projection fieldRef) {
			scan(fieldRef.getObject());
			// TODO Pass on the presence of the field ref so it can be highlighted as such
			// TODO Using data flow analysis, report the set of possible definitions of field references in the projection
			// TODO Report identifier references in the projection as field references
			return null;
		}

		@Override
		@Nullable
		public Void functionLiteral(FunctionLiteral functionLiteral) {
			final int newParameterScopeDepth = this.parameterScopeDepth+1;
			TreeMap<String, DefInfo> newEnvironment = this.environment;
			for(final FunArg arg : functionLiteral.getArgs()) {
				final int argNameSourceOffset = this.exprSourceOffset + arg.getOffsetInParent() + arg.getName().getOffsetInParent();
				newEnvironment = def(arg.getName(), argNameSourceOffset, DefType.PARAMETER, newParameterScopeDepth, newEnvironment);
			}

			final CoreExpr guarantee = functionLiteral.getGuarantee();
			scan(guarantee, this.exprSourceOffset + guarantee.getOffsetInParent(), newParameterScopeDepth, this.objectDepth, this.letDepth, newEnvironment);
			for(final FunArg arg : functionLiteral.getArgs()) {
				if(arg.hasAssertion())
					scan(arg.getAssertion(), this.exprSourceOffset + arg.getOffsetInParent() + arg.getAssertion().getOffsetInParent());
			}
			scan(functionLiteral.getBody(), this.exprSourceOffset + functionLiteral.getBody().getOffsetInParent(), newParameterScopeDepth, this.objectDepth, this.letDepth, newEnvironment);
			return null;
		}

		@Override
		@Nullable
		public Void objectLiteral(ObjectLiteral objectLiteral) {
			final int newObjectDepth = this.objectDepth+1;
			TreeMap<String, DefInfo> newEnvironment = this.environment;
			// Still debating whether to allow access to fields without using a field reference to self
			for(final Method f : objectLiteral.getFields().values()) {
				final int nameSourceOffset = this.exprSourceOffset + f.getOffsetInObject() + f.getKey().getOffsetInParent();
				newEnvironment = def(f.getKey(), nameSourceOffset, fieldDefType(f), newObjectDepth, newEnvironment);
			}
			for(final Method f : objectLiteral.getFields().values()) {
				scan(f.getImplementation(), this.exprSourceOffset + f.getOffsetInObject() + f.getImplementation().getOffsetInParent(), this.parameterScopeDepth, newObjectDepth, this.letDepth, newEnvironment);
			}
			return null;
		}

		private DefType fieldDefType(Method f) {
			final DefType defType = f.getImplementation().acceptVisitor(ScanningExprVisitor.fieldDefTypeCalculator);
			if(defType == null) throw new NullPointerException();
			return defType;
		}

		@Override
		@Nullable
		public Void let(Let let) {
			final int newLetDepth = this.letDepth+1;
			final TreeMap<String, DefInfo> newEnvironment = def(let.getName(), this.exprSourceOffset+let.getName().getOffsetInParent(), letDefType(let), newLetDepth, this.environment);
			scan(let.getValue(), this.exprSourceOffset + let.getValue().getOffsetInParent(), this.parameterScopeDepth, this.objectDepth, newLetDepth, newEnvironment);
			return null;
		}

		@Override
		@Nullable
		public Void listLiteral(ListLiteral listLiteral) {
			for(final CoreExpr e : listLiteral.getElements()) {
				scan(nonNull(e));
			}
			return null;
		}

		@Override
		@Nullable
		public Void setLiteral(SetLiteral setLiteral) {
			for(final CoreExpr e : setLiteral.getElements()) {
				scan(nonNull(e));
			}
			return null;
		}

		@Override
		@Nullable
		public Void badExpr(BadExpr badExpr) {
			return null;
		}
	}

	public static class DefFinder implements DefRefVisitor {
		@Nullable
		private DefInfo result;
		final int targetStartOffset;

		public DefFinder(int targetStartOffset) {
			this.targetStartOffset = targetStartOffset;
		}

		@Override
		public void visitRef(DefInfo def, int sourceOffset, Key key) {
			if(sourceOffset == this.targetStartOffset || def.getSourceOffset() == this.targetStartOffset)
				this.result = def;
		}

		@Override
		public void visitDef(DefInfo def) {
			if(def.getSourceOffset() == this.targetStartOffset)
				this.result = def;
		}

		@Override
		public void visitUnresolved(int sourceOffset, Key key) {
			if(sourceOffset == this.targetStartOffset)
				this.result = DefInfo.FREE_VAR;
		}
		public boolean foundIt() {
			return this.result != null;
		}

		@Nullable
		public DefInfo getResult() {
			return this.result;
		}
	}

	/**
	 * Scan only the subtree that contains an element of particular interest
	 */
	static class SearchingExprVisitor extends ScanningExprVisitor {
		private final DefFinder finder;
		private final int targetStartOffset;

		public SearchingExprVisitor(int targetStartOffset) {
			super(new DefFinder(targetStartOffset));
			this.targetStartOffset = targetStartOffset;
			this.finder = (DefFinder) this.visitor;
		}

		public SearchingExprVisitor(int targetStartOffset,
				DefRefVisitor visitor, int newParentExprOffset,
				int newParameterScopeDepth, int newObjectDepth,
				int newLetDepth, TreeMap<String, DefInfo> newEnvironment) {
			super(visitor, newParentExprOffset, newParameterScopeDepth, newObjectDepth, newLetDepth, newEnvironment);
			this.targetStartOffset = targetStartOffset;
			this.finder = (DefFinder) this.visitor;
		}

		@Override
		protected void scan(CoreExpr e, int exprSourceOffset, int newParameterScopeDepth, int newObjectDepth, int newLetDepth, fj.data.TreeMap<String,DefInfo> newEnvironment) {
			if(this.finder.foundIt()) return;
			final int endOffset = exprSourceOffset + e.getSourceLength();
			if(this.targetStartOffset >= exprSourceOffset && this.targetStartOffset < endOffset) {
				super.scan(e, exprSourceOffset, newParameterScopeDepth, newObjectDepth, newLetDepth, newEnvironment);
			}
		}


		@Override
		ScanningExprVisitor descend(int newParentExprOffset, int newParameterScopeDepth, int newObjectDepth, int newLetDepth, fj.data.TreeMap<String,DefInfo> newEnvironment) {
			return new SearchingExprVisitor(this.targetStartOffset, this.visitor, newParentExprOffset, newParameterScopeDepth, newObjectDepth, newLetDepth, newEnvironment);
		}



		@Nullable
		public DefInfo getResult() {
			return this.finder.getResult();
		}
	}

	public static final class LocalDefTypeCalculator extends
	BaseCoreExprVisitor<DefType> {
		final DefType valueType;
		final DefType constType;
		final DefType functionType;

		public LocalDefTypeCalculator(DefType valueType, DefType constType,
				DefType functionType) {
			super();
			this.valueType = valueType;
			this.constType = constType;
			this.functionType = functionType;
		}

		@Override
		public DefType fallback(CoreExpr unsupported) {
			return this.valueType;
		}

		@Override
		public DefType stringLiteral(StringLiteral stringLiteral) {
			return this.constType;
		}

		@Override
		public DefType numberLiteral(
				NumberLiteral numberLiteral) {
			return this.constType;
		}

		@Override
		public DefType functionLiteral(FunctionLiteral functionLiteral) {
			return this.functionType;
		}

		@Override
		public DefType objectLiteral(ObjectLiteral objectLiteral) {
			for(final Method f : objectLiteral.getFields().values()) {
				final DefType fieldDefType = f.getImplementation().acceptVisitor(this);
				if(fieldDefType == this.valueType) {
					return this.valueType;
				}
			}
			return this.constType;
		}

		@Override
		public @Nullable DefType let(Let let) {
			return let.getValue().acceptVisitor(this);
		}

		@Override
		public DefType listLiteral(ListLiteral listLiteral) {
			for(final CoreExpr elt : listLiteral.getElements()) {
				final DefType eltDefType = elt.acceptVisitor(this);
				if(eltDefType != this.constType)
					return this.valueType;
			}
			return this.constType;
		}

		@Override
		public DefType setLiteral(SetLiteral setLiteral) {
			for(final CoreExpr elt : setLiteral.getElements()) {
				final DefType eltDefType = elt.acceptVisitor(this);
				if(eltDefType != this.constType)
					return this.valueType;
			}
			return this.constType;
		}

	}

	public static @Nullable DefInfo findDef(CoreExpr root, int offset) {
		final SearchingExprVisitor searcher = new SearchingExprVisitor(offset);
		root.acceptVisitor(searcher);
		return searcher.getResult();
	}
	public void scan(CoreExpr expr, final DefRefVisitor visitor) {
		expr.acceptVisitor(new ScanningExprVisitor(visitor));
	}
	public void buildTokenDefMap(CoreExpr expr, final Map<Integer, DefInfo> defs, final Map<Integer, DefInfo> refs, final Set<Integer> unusedDefs) {
		scan(expr, new DefRefVisitor() {

			@Override
			public void visitRef(DefInfo def, int sourceOffset, Key key) {
				refs.put(sourceOffset, def);
				unusedDefs.remove(sourceOffset);
			}

			@Override
			public void visitDef(DefInfo def) {
				defs.put(def.getSourceOffset(), def);
				unusedDefs.add(def.getSourceOffset());
			}

			@Override
			public void visitUnresolved(int sourceOffset, Key key) {
				refs.put(sourceOffset, DefInfo.FREE_VAR);
			}
		});
	}
	public void updateTokenDefMap(CoreExpr oldTree, CoreExpr newTree, final Map<Integer, DefInfo> defs, final Map<Integer, DefInfo> refs, final Set<Integer> unusedDefs) {
		defs.clear();
		refs.clear();
		unusedDefs.clear();
		// TODO Incremental update of the map ... is that feasible ?
		buildTokenDefMap(newTree, defs, refs, unusedDefs);
	}
	public @Nullable <T> T scanTokens(final ParserReader in, CoreExpr expr, final DefRefTokenVisitor<T> visitor) {
		final DefRefTokenAnnotator<T> helper = new DefRefTokenAnnotator<T>(visitor, expr);
		try {
			return this.scanner.scan(in, helper);
		} catch (final IOException e) {
			throw new UnexpectedIOExceptionError(e);
		}
	}

	public @Nullable <T> T nextToken(final ParserReader in, CoreExpr expr, final DefRefTokenVisitor<T> visitor) {
		final DefRefTokenAnnotator<T> helper = new DefRefTokenAnnotator<T>(visitor, expr);
		try {
			return this.scanner.next(in, helper);
		} catch (final IOException e) {
			throw new UnexpectedIOExceptionError(e);
		}
	}

}
