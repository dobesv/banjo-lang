package banjo.expr;

import banjo.expr.source.Precedence;
import banjo.expr.util.OrdUtil;
import banjo.expr.util.SourceFileRange;
import fj.Ord;
import fj.data.Set;



public interface Expr {
	public static final Ord<Expr> EXPR_RANGES_ORD = SourceFileRange.SET_ORD.contramap(x -> x.getRanges());

	public void toSource(StringBuffer sb);

    /**
     * Serialize this expression into a source code buffer, adding parens if
     * necessary.
     * 
     * Parens are added if the outer precedence is higher than the precedence of
     * the expression we are serializing.
     * 
     * Note that converting to / from source code will not preserve the source
     * code ranges information.
     * 
     * @param sb
     *            Buffer to write the expression into
     * @param outerPrec
     *            Precedence of the containing expression
     */
    public default void toSource(StringBuffer sb, Precedence outerPrec) {
        final Precedence prec = getPrecedence();
        final boolean needParens = prec != Precedence.ATOM && outerPrec != prec && outerPrec.isHigherThan(prec);
        if (needParens)
            sb.append('(');
        toSource(sb);
        if (needParens)
            sb.append(')');
    }

    /**
     * Serialize this expression into a source code string, adding parens if
     * necessary, and return a string containing the result.
     * 
     * Parens are added if the outer precedence is higher than the precedence of
     * the expression we are serializing.
     * 
     * Note that converting to / from source code will not preserve the source
     * code ranges information.
     * 
     * @param prec
     *            Precedence of the containing expression
     */
    public default String toSource(Precedence prec) {
        final StringBuffer buf = new StringBuffer();
        toSource(buf, prec);

        final String result = buf.toString();
        return result;
    }

    /**
     * Serialize this expression into a source code string.
     */
    public default String toSource() {
        return toSource(Precedence.lowest());
    }

    /**
     * Get the precedence of this expression. If this expression will be
     * serialized to source code, the precedence can be used to determine
     * whether to put parentheses around it.
     */
	public Precedence getPrecedence();

    /**
     * Get the source location(s) that made up this expression during parsing.
     */
	public Set<SourceFileRange> getRanges();

    /**
     * Construct an Ord instance for comparing objects which uses the given Ord
     * and in addition compares the file ranges of the expr. Normally Ord
     * instances for Expr subclasses ignore the file ranges.
     */
	public static <A extends Expr> Ord<A> ordWithFileRanges(Ord<A> subclassOrd) {
		return OrdUtil.chain(EXPR_RANGES_ORD, subclassOrd);
	}

}