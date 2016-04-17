package banjo.expr.free;

import java.util.function.BiFunction;

import banjo.eval.environment.Environment;
import banjo.value.Value;
import fj.data.List;

/**
 * A free expression is an expression not yet bound to an environment.  Once bound to the environment
 * the expression becomes an (unevaluated) value.
 */
public interface FreeExpression extends BiFunction<Environment, List<Value>, Value> {
}
