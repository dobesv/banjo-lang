package banjo.eval.coreexpr;

import java.util.Map;
import java.util.stream.Stream;

import fj.data.TreeMap;
import banjo.dom.core.CoreExpr;
import banjo.dom.token.Key;
import banjo.eval.ProjectLoader;

/**
 * Interpreter operates by translating between source expressions.
 *
 * The final result
 * @author Dobes
 *
 */
public class CoreExprEvaluator {

	public final EvalEnvironment environment;

	public CoreExprEvaluator(EvalEnvironment environment) {
		super();
		this.environment = environment;
	}
	public CoreExprEvaluator(TreeMap<Key, CoreExpr> bindings) {
		super();
		// Arg ... these root environment things cannot refer to one another if I do it this way!!
		// Might have to hack it and mutate the
		this.environment = EvalEnvironment.root(bindings);
	}

	public EvalResult evaluate(CoreExpr expr) {
		return expr.acceptVisitor(environment);
	}

	public static CoreExprEvaluator forSourceFile(String sourceFilePath) {
		return new CoreExprEvaluator(ProjectLoader.loadLocalAndLibraryBindings(sourceFilePath));
	}

	public static EvalResult eval(String src) {
		return CoreExprEvaluator.forSourceFile("-").evaluate(CoreExpr.fromString(src));
	}
}
