package banjo.dom.source;

import static banjo.parser.util.Check.nonNull;

import org.eclipse.jdt.annotation.Nullable;

import banjo.dom.Expr;
import fj.data.Option;

/**
 * An expression as it appeared in the source code, without any desugaring applied.
 */
public interface SourceExpr extends Expr, SourceNode {

	public static final Option<Integer> NOT_A_CHILD = nonNull(Option.<Integer>none());

	@Nullable <T> T acceptVisitor(SourceExprVisitor<T> visitor);

}
