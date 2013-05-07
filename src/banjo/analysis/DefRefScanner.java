package banjo.analysis;

import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.core.BadExpr;
import banjo.dom.core.Call;
import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprVisitor;
import banjo.dom.core.BaseCoreExprVisitor;
import banjo.dom.core.ExprList;
import banjo.dom.core.Field;
import banjo.dom.core.Projection;
import banjo.dom.core.FunArg;
import banjo.dom.core.FunctionLiteral;
import banjo.dom.core.Let;
import banjo.dom.core.ListLiteral;
import banjo.dom.core.ObjectLiteral;
import banjo.dom.core.SetLiteral;
import banjo.dom.token.Identifier;
import banjo.dom.token.Key;
import banjo.dom.token.NumberLiteral;
import banjo.dom.token.OperatorRef;
import banjo.dom.token.StringLiteral;
import banjo.parser.BanjoScanner;
import banjo.parser.errors.UnexpectedIOExceptionError;
import banjo.parser.util.ParserReader;
import fj.data.Option;

public class DefRefScanner {

	private final BanjoScanner scanner = new BanjoScanner();

	public static class ScanningExprVisitor implements CoreExprVisitor<Void> {
		public final LocalDefTypeCalculator localDefTypeCalculator = new LocalDefTypeCalculator(DefType.LOCAL_VALUE, DefType.LOCAL_CONST, DefType.LOCAL_FUNCTION);
		public final LocalDefTypeCalculator fieldDefTypeCalculator = new LocalDefTypeCalculator(DefType.SELF_FIELD, DefType.SELF_CONST, DefType.SELF_METHOD);
		protected final DefRefVisitor visitor;
		final LinkedList<HashMap<String,DefInfo>> environment = new LinkedList<>();
		int parameterScopeDepth = 0;
		int objectDepth = 0;
		int letDepth = 0;

		@Nullable CoreExpr parentExpr;
		int parentExprOffset;

		public ScanningExprVisitor(DefRefVisitor visitor) {
			this.visitor = visitor;
		}

		protected void scan(@Nullable CoreExpr e) {
			if(e == null) return;
			final HashMap<String,DefInfo> scope = new HashMap<>();
			this.environment.push(scope);
			final int oldParentExprOffset = this.parentExprOffset;
			final CoreExpr oldParentExpr = this.parentExpr;
			this.parentExprOffset = sourceOffset(e);
			this.parentExpr = e;
			e.acceptVisitor(this);
			this.parentExpr = oldParentExpr;
			this.parentExprOffset = oldParentExprOffset;
			if(this.environment.pop() != scope) throw new Error();
		}

		protected int sourceOffset(CoreExpr e) {
			final CoreExpr parentExpr = this.parentExpr;
			if(e == parentExpr) return this.parentExprOffset;
			if(parentExpr == null) return 0;
			return this.parentExprOffset + parentExpr.getSourceExpr().offsetToChild(e.getSourceExpr()).orSome(0);
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
			for(final HashMap<String,DefInfo> scope : this.environment) {
				final DefInfo def = scope.get(key.getKeyString());
				if(def != null) {
					this.visitor.visitRef(def, sourceOffset(key), key);
					return null;
				}
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
			// TODO - resolve operators to functions and report the location of the referenced function as the definition
			return null;
		}

		@Override
		@Nullable
		public Void call(Call call) {
			scan(call.getCallee());
			for(final CoreExpr arg : call.getArguments()) {
				scan(arg);
			}
			return null;
		}

		@Override
		@Nullable
		public Void exprList(ExprList exprList) {
			final HashMap<String,DefInfo> scope = new HashMap<>();
			this.environment.push(scope);
			final int scopeDepth = this.letDepth;
			this.letDepth++;
			for(final CoreExpr e : exprList.getElements()) {
				if(e instanceof Let) {
					final Let let = (Let)e;
					final Key name = let.getName();
					final DefType defType = letDefType(let);
					def(name, defType, scope, scopeDepth);
					scan(let.getValue());
				} else {
					if(e == null) throw new NullPointerException();
					scan(e);
				}
			}
			this.letDepth--;
			if(this.environment.pop() != scope) throw new Error();
			return null;
		}

		private void def(Key name, DefType defType, HashMap<String, DefInfo> scope, int scopeDepth) {
			final DefInfo def = new DefInfo(name, sourceOffset(name), defType, scopeDepth);
			scope.put(name.getKeyString(), def);
			this.visitor.visitDef(def);
		}

		private DefType letDefType(Let let) {
			final DefType defType = let.getValue().acceptVisitor(this.localDefTypeCalculator);
			if(defType == null) throw new NullPointerException();
			return defType;
		}

		@Override
		@Nullable
		public Void projection(Projection fieldRef) {
			scan(fieldRef.getBase());
			// TODO Pass on the presence of the field ref so it can be highlighted as such
			// TODO Using data flow analysis, report the set of possible definitions of that field
			return null;
		}

		@Override
		@Nullable
		public Void functionLiteral(FunctionLiteral functionLiteral) {
			final HashMap<String,DefInfo> scope = new HashMap<>();
			this.environment.push(scope);
			final int scopeDepth = this.parameterScopeDepth;
			final boolean hasSelfName = functionLiteral.getSelfName() != null;
			this.parameterScopeDepth++;
			for(final FunArg arg : functionLiteral.getArgs()) {
				def(arg.getName(), DefType.PARAMETER, scope, scopeDepth);
			}
			if(hasSelfName) {
				final Key selfName = functionLiteral.getSelfName();
				if(selfName == null) throw new NullPointerException();
				def(selfName, DefType.SELF, scope, scopeDepth);
			}
			scan(functionLiteral.getGuarantee());
			for(final FunArg arg : functionLiteral.getArgs()) {
				if(arg.hasContract())
					scan(arg.getAssertion());
			}
			scan(functionLiteral.getBody());
			this.parameterScopeDepth--;
			if(this.environment.pop() != scope) throw new Error();
			return null;
		}

		@Override
		@Nullable
		public Void objectLiteral(ObjectLiteral objectLiteral) {
			final HashMap<String,DefInfo> scope = new HashMap<>();
			this.environment.push(scope);
			final int scopeDepth = this.objectDepth;
			this.objectDepth++;
			for(final Field f : objectLiteral.getFields().values()) {
				def(f.getKey(), fieldDefType(f), scope, scopeDepth);
			}
			for(final Field f : objectLiteral.getFields().values()) {
				scan(f.getValue());
			}
			this.objectDepth--;
			if(this.environment.pop() != scope) throw new Error();
			return null;
		}

		private DefType fieldDefType(Field f) {
			final DefType defType = f.getValue().acceptVisitor(this.fieldDefTypeCalculator);
			if(defType == null) throw new NullPointerException();
			return defType;
		}

		@Override
		@Nullable
		public Void let(Let let) {
			final HashMap<String,DefInfo> scope = new HashMap<>();
			this.environment.push(scope);
			def(let.getName(), letDefType(let), scope, this.letDepth);
			scan(let.getValue());
			if(this.environment.pop() != scope) throw new Error();
			return null;
		}

		@Override
		@Nullable
		public Void listLiteral(ListLiteral listLiteral) {
			for(final CoreExpr e : listLiteral.getElements()) {
				scan(e);
			}
			return null;
		}

		@Override
		@Nullable
		public Void setLiteral(SetLiteral setLiteral) {
			for(final CoreExpr e : setLiteral.getElements()) {
				scan(e);
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

		@Override
		protected void scan(@Nullable CoreExpr e) {
			if(e == null || this.finder.foundIt()) return;
			final int sourceOffset = sourceOffset(e);
			final int endOffset = sourceOffset + e.getSourceExpr().getSourceLength();
			if(this.targetStartOffset >= sourceOffset && this.targetStartOffset < endOffset) {
				super.scan(e);
			}
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
			for(final Field f : objectLiteral.getFields().values()) {
				final DefType fieldDefType = f.getValue().acceptVisitor(this);
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

	public static @Nullable DefInfo findDef(CoreExpr root, Key target) {
		final Option<Integer> offset = root.getSourceExpr().offsetToChild(target.getSourceExpr());
		if(offset.isNone())
			return null; // Not found ... not even in that AST!
		final SearchingExprVisitor searcher = new SearchingExprVisitor(offset.some());
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
				unusedDefs.add(sourceOffset);
			}

			@Override
			public void visitDef(DefInfo def) {
				defs.put(def.getSourceOffset(), def);
				unusedDefs.add(def.getSourceOffset());
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
