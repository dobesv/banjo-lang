package banjo.eval.expr.test;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import banjo.expr.core.CoreExpr;
import banjo.expr.source.SourceErrorGatherer;
import banjo.expr.source.SourceExpr;
import fj.data.List;

public abstract class BaseSourceExprTest extends BaseExprTest {

	public final String src;
	public SourceExpr parseTree;

	public BaseSourceExprTest(String src) {
		this.src = src;
    }

	@Override
	public CoreExpr getAst() {
		parsesWithoutErrors();
		return CoreExpr.fromSourceExpr(parseTree);
	}
	
	@Test
    public void parsesWithoutErrors() {
		parseTree = SourceExpr.fromString(src);
    	assertEquals(List.nil(), SourceErrorGatherer.getProblems(parseTree));
    }
}