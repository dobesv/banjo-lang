package banjo.eval.expr.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.nio.file.Paths;

import org.junit.Assert;
import org.junit.Test;

import banjo.expr.core.BaseCoreExprVisitor;
import banjo.expr.core.CoreErrorGatherer;
import banjo.expr.core.CoreExpr;
import banjo.expr.core.CoreExprNormalizer;
import banjo.expr.core.ScopedExpr;
import banjo.expr.source.Operator;
import banjo.expr.token.Identifier;
import banjo.expr.token.StringLiteral;
import banjo.expr.util.SourceFileRange;
import fj.data.List;
import fj.data.Set;

public abstract class BaseExprTest {

	final static CoreExprNormalizer normalizer = CoreExprNormalizer.forProjectContainingPath(Paths.get(""));

	CoreExpr expr;
	CoreExpr normalizedExpr;
	CoreExpr trueExpr = normalizer.apply(Identifier.TRUE);
	
	public abstract CoreExpr getAst();

	public void noParseErrors() {
		this.expr = getAst();
		assertEquals(List.nil(), CoreErrorGatherer.problems(this.expr));
	}

	public void normalizes() {
		noParseErrors();
		this.normalizedExpr = normalizer.apply(expr);
		assertNotNull(normalizedExpr);
		normalizedExpr.acceptVisitor(new BaseCoreExprVisitor<Void>() {

			@Override
			public Void fallback() {
				// No problem
				return null;
			}

			@Override
			public Void scoped(ScopedExpr projection) {
				if (projection.getObject().eql(Identifier.FAIL)) {
					Assert.fail("Normalized value references FAIL: " + normalizedExpr.toSource());
				}
				return null;
			}

			@Override
			public Void identifier(Identifier n) {
				if (normalizedExpr.eql(Identifier.FAIL)) {
					Assert.fail("Normalized value is FAIL");
				}
				return null;
			}
		});
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
		CoreExpr trueToken = new StringLiteral("tested true");
		CoreExpr truthTestResult = normalizer.apply(ScopedExpr.callBinaryOp(normalizedExpr, Operator.LOGICAL_AND, trueToken));
		assertTrue(truthTestResult.toSource(), trueToken.eql(truthTestResult));
	}
}