package banjo.analysis;

import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.BadExpr;
import banjo.dom.Call;
import banjo.dom.Comment;
import banjo.dom.CoreExpr;
import banjo.dom.CoreExprVisitor;
import banjo.dom.CoreExprVisitorWithDefault;
import banjo.dom.Ellipsis;
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
import banjo.dom.TokenVisitor;
import banjo.dom.UnitRef;
import banjo.dom.Whitespace;
import banjo.parser.BanjoScanner;
import banjo.parser.errors.UnexpectedIOExceptionError;
import banjo.parser.util.FilePos;
import banjo.parser.util.ParserReader;

public class DefRefScanner {

	

	private final BanjoScanner scanner = new BanjoScanner();

	public final class DefRefTokenScanHelper<T> implements TokenVisitor<T> {
		final CoreExpr rootExpr;
		boolean eof;
		
		@Nullable
		T result;
		private DefRefTokenVisitor<T> visitor;

		public DefRefTokenScanHelper(DefRefTokenVisitor<T> visitor, CoreExpr rootExpr) {
			this.visitor = visitor;
			this.rootExpr = rootExpr;
		}

		public @Nullable T visitStringLiteral(StringLiteral stringLiteral) {
			return visitor.visitStringLiteral(stringLiteral);
		}

		public @Nullable T visitWhitespace(Whitespace ws) {
			return visitor.visitWhitespace(ws);
		}

		public @Nullable T visitNumberLiteral(NumberLiteral numberLiteral) {
			return visitor.visitNumberLiteral(numberLiteral);
		}

		public @Nullable T visitComment(Comment c) {
			return visitor.visitComment(c);
		}

		public @Nullable T visitEllipsis(Ellipsis ellipsis) {
			return visitor.visitEllipsis(ellipsis);
		}

		public @Nullable T visitEof(FilePos endPos) {
			eof = true;
			return visitor.visitEof(endPos);
		}

		public @Nullable T visitUnit(UnitRef unit) {
			return visitor.visitUnit(unit);
		}

		public @Nullable T visitIdentifier(Identifier identifier) {
			DefInfo def = findDef(rootExpr, identifier);
			if(def == null)
				return visitor.visitIdentifier(identifier);
			if(def.getNameToken().getStartOffset() == identifier.getStartOffset())
				return visitIdentifierDef(identifier, def);
			return visitIdentifierRef(identifier, def);
		}

		public @Nullable T visitOperator(OperatorRef operatorRef) {
			return visitor.visitOperator(operatorRef);
		}

		public @Nullable T visitIdentifierDef(Identifier identifier, DefInfo def) {
			return visitor.visitIdentifierDef(identifier, def);
		}

		public @Nullable T visitIdentifierRef(Identifier identifier, DefInfo def) {
			return visitor.visitIdentifierRef(identifier, def);
		}
	}

	public static class ScanningExprVisitor implements CoreExprVisitor<Void> {
		public final LocalDefTypeCalculator localDefTypeCalculator = new LocalDefTypeCalculator(DefType.LOCAL_VALUE, DefType.LOCAL_CONST, DefType.LOCAL_FUNCTION);
		public final LocalDefTypeCalculator fieldDefTypeCalculator = new LocalDefTypeCalculator(DefType.SELF_FIELD, DefType.SELF_CONST, DefType.SELF_METHOD);
		protected final DefRefVisitor visitor;
		final LinkedList<HashMap<String,DefInfo>> environment = new LinkedList<>();
		int scopeDepth = 0;

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
			for(CoreExpr e : exprList.getElements()) {
				if(e instanceof Let) {
					Let let = (Let)e;
					Key name = let.getName();
					DefType defType = letDefType(let);
					def(name, defType, scope);
					scan(let.getValue());
				} else {
					if(e == null) throw new NullPointerException();
					scan(e);
				}
			}
			if(environment.pop() != scope) throw new Error();
			return null;
		}

		private void def(Key name, DefType defType, HashMap<String, DefInfo> scope) {
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
			scopeDepth++;
			for(FunArg arg : functionLiteral.getArgs()) {
				def(arg.getName(), DefType.PARAMETER, scope);
			}
			if(functionLiteral.getSelfName().isSome()) {
				Key selfName = functionLiteral.getSelfName().some();
				if(selfName == null) throw new NullPointerException();
				def(selfName, DefType.SELF, scope);
			}
			if(functionLiteral.hasContract())
				scan(functionLiteral.getContract());
			for(FunArg arg : functionLiteral.getArgs()) {
				if(arg.hasContract())
					scan(arg.getContract());
			}
			scan(functionLiteral.getBody());
			scopeDepth--;
			if(environment.pop() != scope) throw new Error();
			return null;
		}

		@Override
		@Nullable
		public Void visitObjectLiteral(ObjectLiteral objectLiteral) {
			HashMap<String,DefInfo> scope = new HashMap<>();
			environment.push(scope);
			scopeDepth++;
			for(Field f : objectLiteral.getFields().values()) {
				def(f.getKey(), fieldDefType(f), scope);
			}
			for(Field f : objectLiteral.getFields().values()) {
				scan(f.getValue());
			}
			scopeDepth--;
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
			def(let.getName(), letDefType(let), scope);
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
	
	public @Nullable <T> T scanTokens(final ParserReader in, CoreExpr expr, final DefRefTokenVisitor<T> visitor) {
		DefRefTokenScanHelper<T> helper = new DefRefTokenScanHelper<T>(visitor, expr);
		try {
			return scanner.scan(in, helper);
		} catch (IOException e) {
			throw new UnexpectedIOExceptionError(e);
		}
	}
	
	public @Nullable <T> T nextToken(final ParserReader in, CoreExpr expr, final DefRefTokenVisitor<T> visitor) {
		DefRefTokenScanHelper<T> helper = new DefRefTokenScanHelper<T>(visitor, expr);
		try {
			return scanner.next(in, helper);
		} catch (IOException e) {
			throw new UnexpectedIOExceptionError(e);
		}
	}
	
}
