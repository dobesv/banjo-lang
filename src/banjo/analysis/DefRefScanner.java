package banjo.analysis;

import java.util.HashMap;
import java.util.LinkedList;

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

public class DefRefScanner {

	
	public final LocalDefTypeCalculator localDefTypeCalculator = new LocalDefTypeCalculator(DefType.LOCAL_VALUE, DefType.LOCAL_CONST, DefType.LOCAL_FUNCTION);
	public final LocalDefTypeCalculator fieldDefTypeCalculator = new LocalDefTypeCalculator(DefType.SELF_FIELD, DefType.SELF_CONST, DefType.SELF_METHOD);

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

	public void scan(CoreExpr expr, final DefRefVisitor visitor) {
		expr.acceptVisitor(new CoreExprVisitor<Void>() {
			final LinkedList<HashMap<String,DefInfo>> environment = new LinkedList<>();
			int scopeDepth = 0;
			private void scan(CoreExpr e) {
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
			
		});
	}
}
