package banjo.analysis;

import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.dom.Call;
import banjo.dom.CoreExpr;
import banjo.dom.CoreExprVisitor;
import banjo.dom.CoreExprVisitorWithDefault;
import banjo.dom.ExprList;
import banjo.dom.Field;
import banjo.dom.FieldRef;
import banjo.dom.FunArg;
import banjo.dom.FunctionLiteral;
import banjo.dom.Identifier;
import banjo.dom.Key;
import banjo.dom.Let;
import banjo.dom.ListLiteral;
import banjo.dom.NumberLiteral;
import banjo.dom.ObjectLiteral;
import banjo.dom.OperatorRef;
import banjo.dom.SetLiteral;
import banjo.dom.StringLiteral;
import banjo.parser.BanjoScanner;
import banjo.parser.errors.UnexpectedIOExceptionError;
import banjo.parser.util.FileRange;
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

		public ScanningExprVisitor(DefRefVisitor visitor) {
			this.visitor = visitor;
		}

		protected void scan(CoreExpr e) {
			HashMap<String,DefInfo> scope = new HashMap<>();
			environment.push(scope);
			e.acceptVisitor(this);
			if(environment.pop() != scope) throw new Error();
		}

		@Override
		@Nullable
		public Void visitStringLiteral(StringLiteral stringLiteral) {
			return null;
		}

		@Override
		@Nullable
		public Void visitNumberLiteral(NumberLiteral numberLiteral) {
			return null;
		}

		@Nullable
		public Void visitRef(Key key) {
			for(HashMap<String,DefInfo> scope : environment) {
				DefInfo def = scope.get(key.getKeyString());
				if(def != null) {
					visitor.visitRef(def, key);
					return null;
				}
			}
			return null;
		}

		@Override
		@Nullable
		public Void visitIdentifier(Identifier identifier) {
			return visitRef(identifier);
		}

		@Override
		@Nullable
		public Void visitOperator(OperatorRef operatorRef) {
			// TODO - resolve operators to functions and report the location of the referenced function as the definition
			return null;
		}

		@Override
		@Nullable
		public Void visitCall(Call call) {
			scan(call.getCallee());
			for(CoreExpr arg : call.getArguments()) {
				scan(arg);
			}
			return null;
		}

		@Override
		@Nullable
		public Void visitExprList(ExprList exprList) {
			HashMap<String,DefInfo> scope = new HashMap<>();
			environment.push(scope);
			final int scopeDepth = letDepth;
			letDepth++;
			for(CoreExpr e : exprList.getElements()) {
				if(e instanceof Let) {
					Let let = (Let)e;
					Key name = let.getName();
					DefType defType = letDefType(let);
					def(name, defType, scope, scopeDepth);
					scan(let.getValue());
				} else {
					if(e == null) throw new NullPointerException();
					scan(e);
				}
			}
			letDepth--;
			if(environment.pop() != scope) throw new Error();
			return null;
		}

		private void def(Key name, DefType defType, HashMap<String, DefInfo> scope, int scopeDepth) {
			DefInfo def = new DefInfo(name, defType, scopeDepth);
			scope.put(name.getKeyString(), def);
			visitor.visitDef(def);
		}

		private DefType letDefType(Let let) {
			DefType defType = let.getValue().acceptVisitor(localDefTypeCalculator);
			if(defType == null) throw new NullPointerException();
			return defType;
		}

		@Override
		@Nullable
		public Void visitFieldRef(FieldRef fieldRef) {
			scan(fieldRef.getBase());
			// TODO Pass on the presence of the field ref so it can be highlighted as such
			// TODO Using data flow analysis, report the set of possible definitions of that field
			return null;
		}

		@Override
		@Nullable
		public Void visitFunctionLiteral(FunctionLiteral functionLiteral) {
			HashMap<String,DefInfo> scope = new HashMap<>();
			environment.push(scope);
			final int scopeDepth = parameterScopeDepth;
			boolean hasSelfName = functionLiteral.getSelfName().isSome();
			this.parameterScopeDepth++;
			for(FunArg arg : functionLiteral.getArgs()) {
				def(arg.getName(), DefType.PARAMETER, scope, scopeDepth);
			}
			if(hasSelfName) {
				Key selfName = functionLiteral.getSelfName().some();
				if(selfName == null) throw new NullPointerException();
				def(selfName, DefType.SELF, scope, scopeDepth);
			}
			scan(functionLiteral.getContract());
			for(FunArg arg : functionLiteral.getArgs()) {
				if(arg.hasContract())
					scan(arg.getContract());
			}
			scan(functionLiteral.getBody());
			this.parameterScopeDepth--;
			if(environment.pop() != scope) throw new Error();
			return null;
		}

		private void scan(Option<CoreExpr> optExpr) {
			if(optExpr.isSome()) {
				CoreExpr e = optExpr.some();
				if(e == null) throw new NullPointerException();
				scan(e);
			}
		}

		@Override
		@Nullable
		public Void visitObjectLiteral(ObjectLiteral objectLiteral) {
			HashMap<String,DefInfo> scope = new HashMap<>();
			environment.push(scope);
			final int scopeDepth = objectDepth;
			objectDepth++;
			for(Field f : objectLiteral.getFields().values()) {
				def(f.getKey(), fieldDefType(f), scope, scopeDepth);
			}
			for(Field f : objectLiteral.getFields().values()) {
				scan(f.getValue());
			}
			objectDepth--;
			if(environment.pop() != scope) throw new Error();
			return null;
		}

		private DefType fieldDefType(Field f) {
			DefType defType = f.getValue().acceptVisitor(fieldDefTypeCalculator);
			if(defType == null) throw new NullPointerException();
			return defType;
		}

		@Override
		@Nullable
		public Void visitLet(Let let) {
			HashMap<String,DefInfo> scope = new HashMap<>();
			environment.push(scope);
			def(let.getName(), letDefType(let), scope, letDepth);
			scan(let.getValue());
			if(environment.pop() != scope) throw new Error();
			return null;
		}

		@Override
		@Nullable
		public Void visitListLiteral(ListLiteral listLiteral) {
			for(CoreExpr e : listLiteral.getElements()) {
				scan(e);
			}
			return null;
		}

		@Override
		@Nullable
		public Void visitSetLiteral(SetLiteral setLiteral) {
			for(CoreExpr e : setLiteral.getElements()) {
				scan(e);
			}
			return null;
		}

		@Override
		@Nullable
		public Void visitBadExpr(BadExpr badExpr) {
			return null;
		}
	}
	
	public static class DefRefFinder implements DefRefVisitor {
		@Nullable
		private DefInfo result;
		final Key target;
		final int targetStartOffset;

		public DefRefFinder(Key target) {
			this.target = target;
			this.targetStartOffset = target.getStartOffset();
		}

		@Override
		public void visitRef(DefInfo def, Key key) {
			if(key.getStartOffset() == targetStartOffset || def.getNameToken().getStartOffset() == targetStartOffset)
				result = def;
		}

		@Override
		public void visitDef(DefInfo def) {
			if(def.getNameToken().getStartOffset() == targetStartOffset)
				result = def;
		}

		public boolean foundIt() {
			return result != null;
		}

		@Nullable
		public DefInfo getResult() {
			return result;
		}
	}

	/**
	 * Scan only the subtree that contains an element of particular interest
	 */
	static class SearchingExprVisitor extends ScanningExprVisitor {
		final Key target;
		private DefRefFinder finder;
		
		public SearchingExprVisitor(Key target) {
			super(new DefRefFinder(target));
			this.target = target;
			this.finder = (DefRefFinder)visitor;
		}
		
		@Override
		protected void scan(CoreExpr e) {
			if(!finder.foundIt() && e.getFileRange().containsOffset(target.getStartOffset())) {
				super.scan(e);
			}
		}
		


		@Nullable
		public DefInfo getResult() {
			return finder.getResult();
		}
	}

	public static final class LocalDefTypeCalculator extends
			CoreExprVisitorWithDefault<DefType> {
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
			return valueType;
		}

		@Override
		public DefType visitStringLiteral(StringLiteral stringLiteral) {
			return constType;
		}

		@Override
		public DefType visitNumberLiteral(
				NumberLiteral numberLiteral) {
			return constType;
		}

		@Override
		public DefType visitFunctionLiteral(FunctionLiteral functionLiteral) {
			return functionType;
		}

		@Override
		public DefType visitObjectLiteral(ObjectLiteral objectLiteral) {
			for(Field f : objectLiteral.getFields().values()) {
				DefType fieldDefType = f.getValue().acceptVisitor(this);
				if(fieldDefType == valueType) {
					return valueType;
				}
			}
			return constType;
		}

		@Override
		public @Nullable DefType visitLet(Let let) {
			return let.getValue().acceptVisitor(this);
		}

		@Override
		public DefType visitListLiteral(ListLiteral listLiteral) {
			for(CoreExpr elt : listLiteral.getElements()) {
				DefType eltDefType = elt.acceptVisitor(this);
				if(eltDefType != constType)
					return valueType;
			}
			return constType;
		}

		@Override
		public DefType visitSetLiteral(SetLiteral setLiteral) {
			for(CoreExpr elt : setLiteral.getElements()) {
				DefType eltDefType = elt.acceptVisitor(this);
				if(eltDefType != constType)
					return valueType;
			}
			return constType;
		}

	}

	public static @Nullable DefInfo findDef(CoreExpr root, Key target) {
		SearchingExprVisitor searcher = new SearchingExprVisitor(target);
		root.acceptVisitor(searcher);
		return searcher.getResult();
	}
	public void scan(CoreExpr expr, final DefRefVisitor visitor) {
		expr.acceptVisitor(new ScanningExprVisitor(visitor));
	}
	public void buildTokenDefMap(CoreExpr expr, final Map<FileRange, DefInfo> result, final Set<FileRange> unusedDefs) {
		scan(expr, new DefRefVisitor() {
			
			@Override
			public void visitRef(DefInfo def, Key key) {
				result.put(key.getFileRange(), def);
				unusedDefs.add(def.getNameToken().getFileRange());
			}
			
			@Override
			public void visitDef(DefInfo def) {
				result.put(def.getNameToken().getFileRange(), def);
				unusedDefs.add(def.getNameToken().getFileRange());
			}
		});
	}
	public void updateTokenDefMap(CoreExpr oldTree, CoreExpr newTree, Map<FileRange,DefInfo> result, Set<FileRange> unusedDefs) {
		result.clear();
		unusedDefs.clear();
		// TODO Incremental update of the map ... is that feasible ?
		buildTokenDefMap(newTree, result, unusedDefs);
	}
	public @Nullable <T> T scanTokens(final ParserReader in, CoreExpr expr, final DefRefTokenVisitor<T> visitor) {
		DefRefTokenAnnotator<T> helper = new DefRefTokenAnnotator<T>(visitor, expr);
		try {
			return scanner.scan(in, helper);
		} catch (IOException e) {
			throw new UnexpectedIOExceptionError(e);
		}
	}
	
	public @Nullable <T> T nextToken(final ParserReader in, CoreExpr expr, final DefRefTokenVisitor<T> visitor) {
		DefRefTokenAnnotator<T> helper = new DefRefTokenAnnotator<T>(visitor, expr);
		try {
			return scanner.next(in, helper);
		} catch (IOException e) {
			throw new UnexpectedIOExceptionError(e);
		}
	}
	
}
