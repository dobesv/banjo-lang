package banjo.expr.free;

import java.util.function.Function;

import banjo.eval.environment.Environment;
import banjo.value.Value;

/**
 * A free expression is an expression not yet bound to an environment.  Once bound to the environment
 * the expression becomes a value.
 */
public interface FreeExpression extends Function<Environment, Value> {
}
