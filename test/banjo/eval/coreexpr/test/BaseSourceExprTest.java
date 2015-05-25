package banjo.eval.coreexpr.test;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import fj.P;
import fj.P3;
import fj.data.List;
import banjo.eval.coreexpr.CoreExprFactory.DesugarResult;
import banjo.expr.core.CoreExpr;
import banjo.expr.source.SourceErrorGatherer;
import banjo.expr.source.SourceExpr;

public abstract class BaseSourceExprTest extends BaseExprTest {

	public final String src;
	public final SourceExpr parseTree;

	static final P3<String,SourceExpr,CoreExpr> parseAndDesugar(String src) {
		SourceExpr parseTree = SourceExpr.fromString(src);
		CoreExpr expr = CoreExpr.fromSourceExpr(parseTree);
		return P.p(src, parseTree, expr);
	}
	public BaseSourceExprTest(String src) {
	    this(parseAndDesugar(src));
    }
	public BaseSourceExprTest(P3<String,SourceExpr,CoreExpr> p) {
		super(p._3());
		this.src = p._1();
		this.parseTree = p._2();
	}

	@Test
    public void parsesWithoutErrors() {
    	assertEquals(List.nil(), SourceErrorGatherer.getProblems(parseTree));
    }
}