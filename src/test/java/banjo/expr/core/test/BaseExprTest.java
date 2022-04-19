package banjo.expr.core.test;

import static org.junit.Assert.assertEquals;

import java.nio.file.Paths;

import org.junit.Test;

import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.CoreErrorGatherer;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprNormalizer;
import banjo.expr.core.ScopedExpr;
import banjo.expr.token.Identifier;
import banjo.expr.token.NumberLiteral;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

public abstract class BaseExprTest {

    final static CoreExprNormalizer normalizer = CoreExprNormalizer.forProjectContainingPath(Paths.get(""));
    CoreExpr expr;
    CoreExpr normalized;

    public abstract CoreExpr getAst();

    public void noParseErrors() {
        this.expr = getAst();
        assertEquals(List.nil(), CoreErrorGatherer.problems(this.expr));
    }

    public void normalizes() {
        noParseErrors();
        CoreExpr e1 = normalizer.apply(expr);
        normalized = normalizer.force(e1);
    }

    @Test
    public void valueToStringWorks() {
        normalizes();
        normalized.toString();
    }

    public String exprSource() {
        return expr.toSource();
    }

    public Set<SourceFileRange> exprRanges() {
        return expr.getRanges();
    }

    @Test
    public void isTrue() throws Throwable {
        normalizes();
        normalizer.apply(new ScopedExpr(normalized, Identifier.ORDINAL)).acceptVisitor(new BaseCoreExprVisitor<Void>() {
            @Override
            public Void fallback() {
                throw new AssertionError(normalized.toSource());
            }

            @Override
            public Void numberLiteral(NumberLiteral n) {
                assertEquals(normalized.toSource(), 1, n.number.intValue());
                return null;
            }
        });
    }
}
