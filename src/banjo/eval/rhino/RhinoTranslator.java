package banjo.eval.rhino;

import static banjo.parser.util.Check.nonNull;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import org.eclipse.jdt.annotation.Nullable;
import org.mozilla.javascript.Token;
import org.mozilla.javascript.ast.AstNode;
import org.mozilla.javascript.ast.Block;
import org.mozilla.javascript.ast.ConditionalExpression;
import org.mozilla.javascript.ast.ErrorNode;
import org.mozilla.javascript.ast.FunctionCall;
import org.mozilla.javascript.ast.FunctionNode;
import org.mozilla.javascript.ast.Name;
import org.mozilla.javascript.ast.ObjectLiteral;
import org.mozilla.javascript.ast.ObjectProperty;
import org.mozilla.javascript.ast.PropertyGet;
import org.mozilla.javascript.ast.ReturnStatement;
import org.mozilla.javascript.ast.StringLiteral;
import org.mozilla.javascript.ast.VariableDeclaration;
import org.mozilla.javascript.ast.VariableInitializer;

import banjo.dom.core.CoreExpr;
import banjo.dom.core.CoreExprAlgebra;
import banjo.parser.util.SourceFileRange;
import fj.F;
import fj.Ord;
import fj.data.List;
import fj.data.TreeMap;

public class RhinoTranslator implements CoreExprAlgebra<RhinoTranslator.Result> {
	public static final String RUNTIME_JS_NAME = "$banjo";
	private static final Name runtimeName() {
		return new Name(-1, RUNTIME_JS_NAME);
	}

	static AstNode runtimeProperty(String name) {
		return new PropertyGet(runtimeName(), new Name(-1, name));
	}

	/**
	 * Most values are lazy, this evaluates a lazy object until it gets a "concrete" value
	 * of some sort.
	 *
	 * <code><pre>
	 * $banjo.forceValue = function(x) {
	 *     while(typeof x == 'function' && x.name == '_thunk')
	 *         x = x();
	 *     return x;
	 * }
	 * </pre></code>
	 */
	private static final AstNode forceValueFunc() { return runtimeProperty("forceValue"); }

	/**
	 * This is the name of any function that is a "thunk".  Functions or
	 * objects that are not a thunk should not have this name.
	 */
	private static final Name thunkFunctionsName() { return new Name(-1, "_thunk"); }

	/**
	 * When a single object literal contains multiple definitions of a method, $chain_methods
	 * is called to chain those definitions together so they can be put into the same object
	 * slot in the javascript object.
	 *
	 * The first parameter is the first definition, and the second parameter is the second
	 * definition.  The second definition will call the first one if it does a super call.
	 *
	 * The method second definition maybe be modified in-place by this call.
	 *
	 * <code><pre>
	 * function $chain_methods(m1, m2) {
	 *     m2.next = m1;
	 *     return m2;
	 * }
	 * </pre></code>
	 */
	private static final AstNode chainMethodsFunc() { return runtimeProperty("chainMethods"); }

	/**
	 *
	 * <code><pre>
	 * function $method(obj, name) {
	 *     return $force(obj)[name];
	 * }
	 * </pre></code>
	 */
	private static final AstNode methodFunc() { return runtimeProperty("method"); }


	/**
	 * Each method definition is a javascript function, but it comes with some extra
	 * properties.  "$try" is a function that can be used to call the method and it
	 * returns an empty list if the target methods fails any preconditions, otherwise
	 * it will return a singleton list with the result.
	 */
	private static final AstNode tryMethodFunc() { return runtimeProperty("tryMethod"); }

	/**
	 * Return a bound function that calls the next implementation of the method, and which
	 * can use nextMethod() on its "this" value to call any successive method.
	 */
	private static final AstNode nextMethodFunc() { return runtimeProperty("nextMethod"); }

	/**
	 * Return a bound function that calls the next implementation of the method, and which
	 * can use nextMethod() on its "this" value to call any successive method, and which uses
	 * the precondition checking logic of "try".
	 */
	private static final AstNode nextTryMethodFunc() { return runtimeProperty("nextTryMethod"); }

	/**
	 * Inspect uses reflection to construct an inspection of the target
	 * object.
	 */
	private static final AstNode inspectFunc() { return runtimeProperty("inspect"); }

	/**
	 * Extend creates a new object from two other ones.  The new object copies
	 * all methods from both objects into a new object, chaining any methods
	 * that exist in both together so they can make a super call ("call next").
	 */
	private static final AstNode extendFunc() { return runtimeProperty("extend"); }

	/**
	 * The "$list" special function takes a constructs a language-internal list
	 * from its arguments (it is a varargs function)
	 */
	private static final AstNode listFunc() { return runtimeProperty("list"); }

	/**
	 * This function takes a javascript string and returns a
	 * language-internal wrapper for it.
	 */
	private static final AstNode stringFunc() { return runtimeProperty("str"); }

	/**
	 * This function takes a pair of javascript strings describing a
	 * number and its suffix and returns a language-internal number
	 * instance.
	 */
	private static final AstNode numberFunc() { return runtimeProperty("num"); }

	/**
	 * A javascript name for anonymous methods.
	 */
	private static final Name anonymousId() { return new Name(-1, "$anonymous"); }

	/**
	 * Take whatever we need to make a method and create a javascript
	 * object out of it that's compatible with the way we represent
	 * objects in Rhino.
	 *
	 * <code><pre>
	 * function $method(checker, main) {
	 *     main.check = checker;
	 *     return main;
	 * }
	 * </pre></code>
	 */
	private static final AstNode methodDefFunc() { return runtimeProperty("methodDef"); }

	/**
	 * Try to figure out if a banjo-internal object is "truthy" or not.
	 *
	 * <code><pre>
	 * function $truthy(x) {
	 *    // op$logical_and is the mangled name for the "&&" method; "&&" should return its argument
	 *    // if the underlying object is truthy, otherwise it will return itself
	 *    try {
	 *        var y = (typeof x.op$logical_and == 'function') && x.op$logical_and(true);
	 *        return (y === true);
	 *    } catch(e) { return false; } // Not truthy-looking
	 * }
	 * </pre></code>
	 */
	private static final AstNode truthyFunc() { return runtimeProperty("truthy"); }


	static class Result {
		static final List<VariableInitializer> EMPTY_LITERALS = List.nil();
		final AstNode node;
		final List<VariableInitializer> literals;

		public Result(AstNode node, List<VariableInitializer> literals) {
			super();
			this.node = node;
			this.literals = literals;
		}

		public Result(AstNode astNode) {
			this(astNode, EMPTY_LITERALS);
		}

		/**
		 * Create a result with the given AST Node and also add
		 * all the literals from the list of results given.
		 *
		 * @param n
		 * @param innerResults
		 */
		public static Result composite(AstNode n, Iterable<Result> innerResults) {
			List<VariableInitializer> newLiterals = EMPTY_LITERALS;
			for(Result r : innerResults) {
				newLiterals = newLiterals.append(r.literals);
			}
			return new Result(n, newLiterals);
		}

		public static Result literal(AstNode node, VariableInitializer prop) {
			return new Result(node, List.single(prop));
		}
	}

	public static String jsIdentifier(String id) {
		StringBuffer escaped = new StringBuffer();
		escaped.append("i$");
		escapeId(id, escaped);
		String idStr = nonNull(escaped.toString());
		return idStr;
	}

	public static void escapeId(String id, StringBuffer escaped) {
		for(char ch : id.toCharArray()) {
			if(Character.isLetter(ch) || Character.isDigit(ch)) {
				escaped.append(ch);
			} else if(ch == ' ') {
				escaped.append('_');
			} else {
				escaped.append('$');
				escaped.append((int)ch);
			}
		}
	}

	public static String escapeId(String id) {
		StringBuffer buf = new StringBuffer();
		escapeId(id, buf);
		return nonNull(buf.toString());
	}


	static List<AstNode> nodes(List<Result> results) {
		return results.map(new F<Result,AstNode>() {
			@Override
			public AstNode f(@Nullable Result a) {
				if(a == null) throw new NullPointerException();
				return a.node;
			}
		});
	}

	@Override
	public Result badExpr(List<SourceFileRange> ranges, String message, Object... args) {
		ErrorNode n = new ErrorNode();
		n.setMessage(MessageFormat.format(message, args));
		return new Result(n);
	}

	@Override
	public Result objectLiteral(List<SourceFileRange> ranges, List<Result> methods) {
		// <ethods should be ObjectProperty instances
		TreeMap<String,ObjectProperty> methodsByName = TreeMap.empty(Ord.stringOrd);
		ObjectLiteral n = new ObjectLiteral();
		for(AstNode m: nodes(methods)) {
			ObjectProperty prop = (ObjectProperty)m;
			Name name = (Name)prop.getLeft();
			ObjectProperty prev = methodsByName.get(name.getIdentifier()).toNull();
			if(prev != null) {
				FunctionCall overload = new FunctionCall();
				overload.setTarget(chainMethodsFunc());
				overload.setArguments(Arrays.asList(prev.getRight(), prop.getRight()));
			} else {
				n.addElement(prop);
			}
		}
		return Result.composite(n, methods);
	}

	@Override
	public Result numberLiteral(List<SourceFileRange> ranges, Number value, String suffix) {
		// Stash the literal in our literals map, return a reference to that.
		// TODO Actually this isn't the value we want to store ... we want to store something that
		// is pimped up with all the right methods from the library!
		Name varName = id("n "+value.toString()+suffix);
		StringLiteral valueStr = stringLiteralExpr(nonNull(value.toString()));
		StringLiteral suffixStr = stringLiteralExpr(suffix);
		FunctionCall n = new FunctionCall();
		n.setTarget(numberFunc());
		n.setArguments(Arrays.<AstNode>asList(valueStr, suffixStr));
		return atomicLiteral(varName, n);
	}

	private StringLiteral stringLiteralExpr(String text) {
		StringLiteral suffixStr = new StringLiteral();
		suffixStr.setValue(text);
		suffixStr.setQuoteCharacter('"');
		return suffixStr;
	}

	/**
	 * String and number literals can be constructed in advance; they don't rely on anything in scope.
	 *
	 * @param key
	 * @param appropriateBanjoObject
	 * @return
	 */
	private Result atomicLiteral(Name varName, AstNode expr) {
		VariableInitializer var = new VariableInitializer();
		var.setTarget(varName);
		var.setInitializer(expr);
		return Result.literal(varName, var);
	}

	@Override
	public Result stringLiteral(List<SourceFileRange> ranges, String text) {
		Name varname = id("s "+text);
		StringLiteral lit = new StringLiteral();
		lit.setQuoteCharacter('"');
		lit.setValue(text);
		FunctionCall n = new FunctionCall();
		n.setTarget(stringFunc());
		n.setArguments(Arrays.<AstNode>asList(lit));
		return atomicLiteral(varname, n);
	}

	@Override
	public Result listLiteral(List<SourceFileRange> ranges, List<Result> elements) {
		FunctionCall n = new FunctionCall();
		n.setTarget(listFunc());
		for(Result r : elements) {
			n.addArgument(r.node);
		}
		return Result.composite(n, elements);
	}

	@Override
	public Result call(List<SourceFileRange> ranges, Result object, Result name, List<List<Result>> argumentLists, boolean optional, boolean callNext) {

		// TODO This is strict calling, not lazy!  Need to create thunks and be able to call into thunks!

		Name methodName = name.node instanceof Name ? (Name)name.node : id(((StringLiteral)name.node).getValue());
		StringLiteral methodNameStrLit = new StringLiteral();
		methodNameStrLit.setValue(methodName.getIdentifier());

		AstNode targetNode = object.node;

		// A callNext invokation changes the target object to one with a replaced method
		if(callNext) {
			FunctionCall wrap = new FunctionCall();
			wrap.setTarget(nextMethodFunc());
			wrap.setArguments(Arrays.asList(targetNode, methodNameStrLit));
			targetNode = wrap;
		}

		FunctionCall n = new FunctionCall();
		if(optional) {
			// $try(target, "methodName")(a1,a2)
			FunctionCall wrap = new FunctionCall();
			wrap.setTarget(tryMethodFunc());
			wrap.setArguments(Arrays.asList(targetNode, methodName));
			n.setTarget(wrap);
		} else {
			// target.methodName(a1,a2)
			FunctionCall wrap = new FunctionCall();
			wrap.setTarget(methodFunc());
			wrap.setArguments(Arrays.asList(targetNode, methodName));
			n.setTarget(wrap);
			@SuppressWarnings("null")
			AstNode method = new PropertyGet(-1, -1, targetNode, methodName);
			n.setTarget(method);
		}
		n.setArguments(Collections.<AstNode>emptyList());

		FunctionCall nn = null;
		for(List<Result> argList : argumentLists) {
			if(argList == null) throw new NullPointerException();
			if(nn != null) {
				n = new FunctionCall();
				n.setTarget(nn);
			}
			n.setArguments(new ArrayList<AstNode>(nodes(argList).toCollection()));
			nn = n;
		}

		FunctionNode thunk = new FunctionNode();
		thunk.setBody(block(returnStmt(n)));
		thunk.setFunctionName(thunkFunctionsName());
		thunk.setFunctionType(FunctionNode.FUNCTION_EXPRESSION);
		return Result.composite(thunk, List.cons(object, List.cons(name, List.join(argumentLists))));
	}

	@Override
	public Result extend(List<SourceFileRange> ranges, Result base, Result extension) {
		FunctionCall n = new FunctionCall();
		n.setTarget(extendFunc());
		n.setArguments(Arrays.<AstNode>asList(base.node, extension.node));
		return Result.composite(n, List.list(base, extension));
	}

	@Override
	public Result inspect(List<SourceFileRange> ranges, Result target) {
		FunctionCall n = new FunctionCall();
		n.setTarget(inspectFunc());
		n.setArguments(Arrays.<AstNode>asList(target.node));
		return new Result(n, target.literals);
	}

	private Block block(AstNode ... stmts) {
		Block result = new Block();
		for(AstNode stmt : stmts) {
			result.addStatement(stmt);
		}
		return result;
	}

	private java.util.List<AstNode> copyArgList(List<Result> args) {
		ArrayList<AstNode> result = new ArrayList<>(args.length());
		for(Result argResult : args) {
			result.add(new Name(-1, ((Name)argResult.node).getIdentifier()));
		}
		return result;
	}
	@Override
	public Result method(List<SourceFileRange> ranges, Result selfArg,
			Result name, List<List<Result>> argumentLists,
			Result precondition, Result body, Result postcondition) {

		// "name" : ($method_defn(function(... args ...) { ... check preconditions ... }, function(... args ...) { ... body ... }))
		AstNode nameNode = name.node;
		@SuppressWarnings("null")
		Name funcName = (nameNode instanceof Name ? (Name)nameNode : id(nonNull((StringLiteral)nameNode).getValue()));

		// Precondition check - "function(...) { return precondition ? $list(body) : $list(); }"
		FunctionNode checker = functionExpr(funcName, block(
			returnStmt(ternary(
				precondition,
				listLiteral(ranges, List.single(body)).node,
				listLiteral(ranges, List.<Result>nil()).node
			))
		));

		// Main function = (function(...) { return precondition ? body : failure_placeholder; }
		// Preconditions are assumed to be verified by the programmer
		FunctionNode main = functionExpr(funcName, block(
			returnStmt(ternary(
				precondition,
				body.node,
				runtimeProperty("failure")
			))
		));

		FunctionNode innerChecker = null, innerMain = null;
		for(List<Result> argList : argumentLists) {
			if(argList == null) throw new NullPointerException();
			if(innerChecker != null) {
				if(innerMain == null) throw new NullPointerException();
				checker = functionExpr(funcName, block(returnStmt(innerChecker)));
				main = functionExpr(funcName, block(returnStmt(innerMain)));
			}
			checker.setParams(copyArgList(argList));
			innerChecker = checker;
			main.setParams(copyArgList(argList));
			innerMain = main;
		}

		FunctionCall maker = new FunctionCall();
		maker.setTarget(methodDefFunc());
		maker.setArguments(Arrays.<AstNode>asList(checker, main));

		ObjectProperty prop = new ObjectProperty();
		prop.setLeftAndRight(nameNode, maker);
		List<Result> subresults = List.list(selfArg, name, precondition, body, postcondition).append(List.join(argumentLists));
		return Result.composite(prop, subresults);
	}

	private FunctionNode functionExpr(Name name, Block body) {
		FunctionNode func = new FunctionNode(-1, new Name(-1, name.getIdentifier()));
		func.setFunctionType(FunctionNode.FUNCTION_EXPRESSION);
		func.setBody(body);
		return func;
	}

	private ReturnStatement returnStmt(AstNode returnValue) {
		return new ReturnStatement(-1, -1, returnValue);
	}

	private ConditionalExpression ternary(Result test, AstNode valueIfTrue,
			@Nullable AstNode valueIfFalse) {
		ConditionalExpression result = new ConditionalExpression();
		result.setTestExpression(truthyCheck(test));
		result.setTrueExpression(valueIfTrue);
		result.setFalseExpression(valueIfFalse);
		return result;
	}

	private FunctionCall truthyCheck(Result precondition) {
		FunctionCall truthyCheck = new FunctionCall();
		truthyCheck.setTarget(truthyFunc());
		truthyCheck.setArguments(Collections.singletonList(precondition.node));
		return truthyCheck;
	}

	private Name id(String id) {
		// TODO Generalize this
		if(id.equals("&&"))
			return new Name(-1, "op$logical_and");

		// Identifiers can contain letters, digits, underscores, and dollar signs, but cannot begin with a number.
		// Identifiers cannot be a reserved word
		return new Name(-1, jsIdentifier(id));
	}

	@Override
	public Result identifier(List<SourceFileRange> ranges, String id) {
		return new Result(id(id));
	}

	@Override
	public Result mixfixFunctionIdentifier(List<SourceFileRange> sourceFileRanges, List<String> parts) {
		StringBuffer escaped = new StringBuffer();
		escaped.append("m$");
		for(String id : parts) {
			escaped.append("$");
			escapeId(nonNull(id), escaped);
		}
		return new Result(new Name(-1, escaped.toString()));
	}

	@Override
	public Result anonymous() {
		return new Result(anonymousId());
	}

	/**
	 * Take a CoreExpr and return a function that takes a reference to the runtime
	 * and returns the value of the expression represented by the function.
	 */
	public FunctionNode translate(CoreExpr coreExpr) {
		Result res = coreExpr.acceptVisitor(this);
		FunctionNode wrapperFunc = new FunctionNode();
		wrapperFunc.setFunctionType(FunctionNode.FUNCTION_EXPRESSION);
		wrapperFunc.addParam(runtimeName());
		Block wrapperBody = new Block();
		if(!res.literals.isEmpty()) {
			VariableDeclaration vars = new VariableDeclaration();
			wrapperBody.addStatement(vars);
			for(VariableInitializer var : res.literals) {
				vars.addVariable(var);
			}
		}
		FunctionCall forced = new FunctionCall();
		forced.setTarget(forceValueFunc());
		forced.addArgument(res.node);
		wrapperBody.addStatement(returnStmt(forced));
		wrapperFunc.setBody(wrapperBody);
		return wrapperFunc;
	}
}
