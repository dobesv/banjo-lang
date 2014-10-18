package banjo.eval.rhino;

import static banjo.parser.util.Check.nonNull;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.Scanner;

import org.eclipse.jdt.annotation.Nullable;
import org.mozilla.javascript.CompilerEnvirons;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.ErrorReporter;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.IRFactory;
import org.mozilla.javascript.Script;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;
import org.mozilla.javascript.ast.AstRoot;
import org.mozilla.javascript.ast.FunctionNode;
import org.mozilla.javascript.ast.ScriptNode;
import org.mozilla.javascript.optimizer.Codegen;

import banjo.dom.core.CoreExpr;
import banjo.eval.rhino.RhinoTranslator.Result;
import banjo.parser.util.UnexpectedIOExceptionError;

public class RhinoEvaluator {
	private final RhinoTranslator translator = new RhinoTranslator();
    private final Codegen compiler = new Codegen();

	public @Nullable Object evaluate(CoreExpr ast) {
		Context ctx = Context.enter();
		try {
			ctx.setOptimizationLevel(-1);
			Function f = compileScript(ast, ctx);
			Scriptable scope = f.getParentScope();
			String rtSource = new Scanner(getClass().getResourceAsStream("rt.js"), "UTF-8").useDelimiter("\\A").next();
			Function rt = ctx.compileFunction(scope, rtSource, "rt.js", 1, null);
			scope.put("$banjo", scope, rt);
			rt.call(ctx, scope, rt, new Object[] {rt});
			return f.call(ctx, scope, scope, new Object[] {rt});
		} finally {
			Context.exit();
		}
	}
	public String toJs(CoreExpr coreExpr) {
		Context ctx = Context.enter();
		try {
			return ctx.decompileFunction(compileScript(coreExpr, ctx), 2);
		} finally {
			Context.exit();
		}
	}
	public Function compileScript(CoreExpr coreExpr, Context ctx) {
		FunctionNode funcNode = translator.translate(coreExpr);
        CompilerEnvirons compilerEnv = new CompilerEnvirons();
        compilerEnv.initFromContext(ctx);
        ErrorReporter compilationErrorReporter = compilerEnv.getErrorReporter();
        IRFactory irf = new IRFactory(compilerEnv, compilationErrorReporter);
        AstRoot astRoot = new AstRoot();
        astRoot.setInStrictMode(true);
        if(coreExpr.getSourceFileRanges().isNotEmpty())
        	astRoot.setSourceName(coreExpr.getSourceFileRanges().head().getSourceFile());
        else
            astRoot.setSourceName("");
        astRoot.addChild(funcNode);
        ScriptNode tree = nonNull(irf.transformTree(astRoot));
        Object bytecode = compiler.compile(compilerEnv, tree, tree.getEncodedSource(), true);
        Object securityDomain = null;
        ScriptableObject scope = ctx.initStandardObjects();
		return nonNull(compiler.createFunctionObject(ctx, scope, bytecode, securityDomain));
	}
}
